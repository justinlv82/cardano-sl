module Cardano.Wallet.Kernel.ChainState (
    -- * Chain state and state modifier
    ChainState(..)
  , ChainStateModifier(..)
  , fromCPS
  , applyChainStateModifier
    -- * Chain brief
  , ChainBrief(..)
  , getChainBrief
    -- * Restoration
  , ChainStateRestoration(..)
  , getChainStateRestoration
  ) where

import           Universum

import qualified Data.Map.Strict as Map
import           Data.SafeCopy (SafeCopy (..))

import           Pos.Chain.Update (BlockVersionData (..),
                     ConfirmedProposalState (..), HasUpdateConfiguration,
                     genesisBlockVersion, genesisSoftwareVersions, ourAppName)
import           Pos.Core (HasConfiguration, ScriptVersion, SlotId)
import           Pos.Core.Block (HeaderHash, headerHash)
import           Pos.Core.Configuration (genesisBlockVersionData)
import           Pos.Core.Slotting (getEpochOrSlot, unEpochOrSlot)
import           Pos.Core.Update (ApplicationName, BlockVersion,
                     BlockVersionModifier (..), NumSoftwareVersion,
                     SoftwareVersion (..), UpdateProposal (..))
import           Pos.DB.BlockIndex (getTipHeader)
import           Pos.DB.Update (getAdoptedBVFull, getConfirmedProposals,
                     getConfirmedSV)

import           Formatting (bprint, build, shown, (%))
import qualified Formatting.Buildable
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util (mapJson)

import           Cardano.Wallet.Kernel.NodeStateAdaptor (LockContext,
                     NodeStateAdaptor, withNodeState)

{-------------------------------------------------------------------------------
  Chain state and state modifiers
-------------------------------------------------------------------------------}

-- | Chain state
--
-- This is an extract from a full chain state, containing only the variables
-- that the wallet is interested in.
data ChainState = ChainState {
      csBlockVersion    :: !BlockVersion
    , csSoftwareVersion :: !SoftwareVersion
    , csScriptVersion   :: !ScriptVersion
    , csMaxTxSize       :: !Byte
    }

-- | Chain state modifier
--
-- This is a summary of a 'ConfirmedProposalState'
data ChainStateModifier = ChainStateModifier {
      csmBlockVersion    :: !BlockVersion
    , csmSoftwareVersion :: !SoftwareVersion
    , csmScriptVersion   :: !(Maybe ScriptVersion)
    , csmMaxTxSize       :: !(Maybe Byte)
    }

-- | The header of the block the proposal got confirmed in and the corresponding
-- 'ChainStateModifier'
fromCPS :: ConfirmedProposalState -> (HeaderHash, ChainStateModifier)
fromCPS ConfirmedProposalState{..} = (cpsConfirmed, ChainStateModifier {
      csmBlockVersion    = upBlockVersion
    , csmSoftwareVersion = upSoftwareVersion
    , csmScriptVersion   = bvmScriptVersion
    , csmMaxTxSize       = bvmMaxTxSize
    })
  where
    UnsafeUpdateProposal{..} = cpsUpdateProposal
    BlockVersionModifier{..} = upBlockVersionMod

-- | Apply a chain state modifier to a chain state
applyChainStateModifier :: ChainStateModifier -> ChainState -> ChainState
applyChainStateModifier ChainStateModifier{..} ChainState{..} = ChainState{
      csBlockVersion    =                           csmBlockVersion
    , csSoftwareVersion =                           csmSoftwareVersion
    , csScriptVersion   = fromMaybe csScriptVersion csmScriptVersion
    , csMaxTxSize       = fromMaybe csMaxTxSize     csmMaxTxSize
    }

{-------------------------------------------------------------------------------
  Chain summary
-------------------------------------------------------------------------------}

-- | Self-consistent summary of the current tip of the chain
data ChainBrief = ChainBrief {
      -- | Header hash of the chain tip, according to the node DB
      cbTip    :: HeaderHash

      -- | Slot ID of the tip
      --
      -- NOTE: This is /not/ the "current" slot ID, but rather the slot
      -- associated with the tip. The intention is that 'ChainBrief' provides a
      -- consistent view of the node's database for restoration purposes. If the
      -- current slot ID (based on timestamp) is needed, use the 'MonadSlots'
      -- interface (but I strongly suspect 'MonadSlots' may in fact only be
      -- needed by the underlying node).
    , cbSlotId :: SlotId

      -- | The chain state at the time of the tip
      --
      -- Implementation note: Although we have no way of verifying that these
      -- actually match up, the hope is that since we read all these values
      -- while locking the node state, that they will be consistent with each
      -- other. That might be overly optimistic.
    , cbState  :: ChainState
    }

-- | Get 'ChainBrief' for current chain tip
getChainBrief :: HasCallStack
              => NodeStateAdaptor IO
              -> LockContext
              -> IO ChainBrief
getChainBrief node lc = withNodeState node $ \withLock -> do
    -- We use 'getCurrentSlotInaccurate' because the documentation says that
    -- this will fall back on the DB
    (tip, (bv, bvd), mSV) <- withLock lc $ \_tip ->
            (,,)
        <$> getTipHeader
        <*> getAdoptedBVFull
        <*> getConfirmedSV ourAppName
    sv <- case mSV of
            Nothing -> throwM $ MissingSoftwareVersion callStack ourAppName
            Just sv -> return sv
    case unEpochOrSlot (getEpochOrSlot tip) of
      Left _epochIndex ->
        -- If we get an epoch index, the tip happens to be an epoch boundary
        -- block or the genesis block. If it's the former, we should somehow
        -- figure out the preceding block instead. If it's the genesis block,
        -- we should return a special value.
        error "getChainBrief: genesis/EBB case not yet implemented"
      Right slotId -> do
        return ChainBrief {
            cbSlotId = slotId
          , cbTip    = headerHash tip
          , cbState  = ChainState {
                csBlockVersion    = bv
              , csScriptVersion   = bvdScriptVersion bvd
              , csMaxTxSize       = bvdMaxTxSize     bvd
              , csSoftwareVersion = SoftwareVersion ourAppName sv
              }
          }


{-------------------------------------------------------------------------------
  Restoration
-------------------------------------------------------------------------------}

data ChainStateRestoration = ChainStateRestoration {
      -- | Initial chain state
      --
      -- This provides a base case for applying the 'ChainStateModifier's
      csrGenesis :: !ChainState

      -- | All updates, indexed by the block in which they were confirmed
    , csrUpdates :: !(Map HeaderHash ChainStateModifier)

      -- | Current chain state
      --
      -- This provides the target for restoration, as well as its starting
      -- point: we synchronously create a checkpoint for the current tip, and then
      -- asynchronously restore the missing checkpoints (possibly from genesis
      -- when we are restoring, or from another checkpoint if we are catching up).
      -- | Current tip
    , csrCurrent :: !ChainBrief
    }

-- | Get all information needed for restoration
getChainStateRestoration :: HasCallStack
                         => NodeStateAdaptor IO
                         -> LockContext
                         -> IO ChainStateRestoration
getChainStateRestoration node lc = do
    current <- getChainBrief node lc
    -- We don't need to lock now -- it's possible (in principle) that there
    -- might be a new proposal at this point, but old proposals should still
    -- exist.
    --
    -- TODO: Unless this removes proposals that get rolled back...?
    withNodeState node $ \_lock -> do
      proposals <- getConfirmedProposals allVersions
      sv <- case genesisSoftwareVersion of
              Just sv -> return sv
              Nothing -> throwM $ MissingSoftwareVersion callStack ourAppName
      return ChainStateRestoration{
            csrGenesis = initChainState sv
          , csrUpdates = Map.fromList $ map fromCPS proposals
          , csrCurrent = current
          }
  where
    -- We want all updates across all versions
    allVersions :: Maybe NumSoftwareVersion
    allVersions = Nothing

    -- Initial chain state (at the start of the blockchain)
    --
    -- At the moment this gets determined by the configuration.yaml file
    initChainState :: HasConfiguration => SoftwareVersion -> ChainState
    initChainState sv = ChainState{
          csBlockVersion    = genesisBlockVersion
        , csSoftwareVersion = sv
        , csScriptVersion   = bvdScriptVersion
        , csMaxTxSize       = bvdMaxTxSize
        }
      where
        BlockVersionData{..} = genesisBlockVersionData

    genesisSoftwareVersion :: HasUpdateConfiguration => Maybe SoftwareVersion
    genesisSoftwareVersion = listToMaybe $ filter isOurs genesisSoftwareVersions

    isOurs :: HasUpdateConfiguration => SoftwareVersion -> Bool
    isOurs sv = svAppName sv == ourAppName

{-------------------------------------------------------------------------------
  Custom exceptions
-------------------------------------------------------------------------------}

data MissingSoftwareVersion = MissingSoftwareVersion CallStack ApplicationName
  deriving Show

instance Exception MissingSoftwareVersion

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

instance SafeCopy ChainBrief where
  getCopy = error "TODO: getCopy for ChainBrief"
  putCopy = error "TODO: putCopy for ChainBrief"

{-------------------------------------------------------------------------------
  Pretty printing
-------------------------------------------------------------------------------}

instance Buildable ChainState where
  build ChainState{..} = bprint
    ( "ChainState "
    % "{ blockVersion:    " % build
    % ", softwareVersion: " % build
    % ", scriptVersion:   " % build
    % ", maxTxSize:       " % shown
    % "}"
    )
    csBlockVersion
    csSoftwareVersion
    csScriptVersion
    csMaxTxSize

instance Buildable ChainStateModifier where
  build ChainStateModifier{..} = bprint
    ( "ChainStateModifier "
    % "{ blockVersion:    " % build
    % ", softwareVersion: " % build
    % ", scriptVersion:   " % build
    % ", maxTxSize:       " % shown
    % "}"
    )
    csmBlockVersion
    csmSoftwareVersion
    csmScriptVersion
    csmMaxTxSize

instance Buildable ChainStateRestoration where
  build ChainStateRestoration{..} = bprint
    ( "ChainStateRestoration "
    % "{ genesis: " % build
    % ", updates: " % mapJson
    % ", current: " % build
    % "}"
    )
    csrGenesis
    csrUpdates
    csrCurrent

instance Buildable ChainBrief where
  build ChainBrief{..} = bprint
    ( "ChainBrief "
    % ", slotId: " % build
    % ", tip:    " % build
    % ", state:  " % build
    % "}"
    )
    cbSlotId
    cbTip
    cbState
