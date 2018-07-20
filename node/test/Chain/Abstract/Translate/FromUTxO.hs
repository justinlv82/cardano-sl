-- | Translation of UTxO DSL into an abstract chain.
module Chain.Abstract.Translate.FromUTxO where

import Chain.Abstract
import Control.Lens.TH (makeLenses)
import Control.Monad.Error.Class (MonadError, throwError)
import UTxO.Interpreter (Interpret(..), Interpretation(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Universum
import qualified UTxO.DSL as DSL

{-------------------------------------------------------------------------------
  Translation context
-------------------------------------------------------------------------------}

data TransCtxt = TransCtxt
  { -- | All actors in the system.
    _tcAddresses :: [Addr]
  }

makeLenses ''TransCtxt

{-------------------------------------------------------------------------------
  Translation into abstract chain
-------------------------------------------------------------------------------}

newtype TranslateT e m a = TranslateT {
      unTranslateT :: ExceptT e (ReaderT TransCtxt m) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError e
           , MonadIO
           )

instance MonadTrans (TranslateT e) where
  lift = TranslateT . lift . lift

type Translate e = TranslateT e Identity

{-------------------------------------------------------------------------------
  Errors that may occur during interpretation
-------------------------------------------------------------------------------}

-- | Interpretation error
data IntException =
    -- | A UTxO transcation has an empty input list.
    IntEmptyInputs
    -- | A UTxO transaction has an empty output list.
  | IntEmptyOutputs
  deriving (Show)

instance Exception IntException

instance Buildable IntException where
  build = bprint shown

{-------------------------------------------------------------------------------
  Interpretation context
-------------------------------------------------------------------------------}

-- | Checkpoint (we create one for each block we translate)
data IntCheckpoint h = IntCheckpoint {
      -- | Slot number of this checkpoint
      icSlotId        :: !SlotId

      -- | Hash of the current block
    , icBlockHash     :: !(h (Block h Addr))

      -- | Running stakes
    , icStakes        :: !(StakeDistribution Addr)

      -- | Delegation graph. This is instantiated to the identify function.
    , icDlg           :: Addr -> Addr
    }


-- | Interpretation context
data IntCtxt h = IntCtxt {
      -- | Transaction map
      _icTx      :: !(Map (h (Transaction h Addr)) (Transaction h Addr))

      -- | Checkpoints
    , _icCheckpoints :: !(NonEmpty (IntCheckpoint h))
    }

makeLenses ''IntCtxt

{-------------------------------------------------------------------------------
  The interpretation monad
-------------------------------------------------------------------------------}

-- | Interpretation monad
newtype IntT h e m a = IntT {
    unIntT :: StateT (IntCtxt h) (TranslateT (Either IntException e) m) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError (Either IntException e)
           )

-- | Evaluate state strictly
instance Monad m => MonadState (IntCtxt h) (IntT h e m) where
  get    = IntT $ get
  put !s = IntT $ put s

-- | Convenience function to list actions in the 'Translate' monad
liftTranslateInt :: Monad m
                 => TranslateT IntException m a
                 -> IntT h e m a
liftTranslateInt ta =  IntT $ lift $ mapTranslateErrors Left $ withConfig $ ta

{-------------------------------------------------------------------------------
  Dealing with the transactions
-------------------------------------------------------------------------------}

-- | Add transaction into the context
putTx :: forall h e m. (DSL.Hash h Addr, Monad m)
          => DSL.Transaction h Addr -> IntT h e m ()
putTx t id = icTxMeta %= Map.insert (DSL.hash t) t

getTx :: (DSL.Hash h Addr, Monad m)
          => h (Transaction h Addr) -> IntT h e m (Transaction h Addr)
getTx h = do
    tx <- use icTx
    case Map.lookup h tx of
      Nothing -> throwError $ Left $ IntUnknownHash (pretty h)
      Just m  -> return m

-- | Lookup a transaction by hash
findHash' :: (DSL.Hash h Addr, Monad m)
          => h (Transaction h Addr) -> IntT h e m (Transaction h Addr)
findHash' = fmap tmTx . getTx

-- | Resolve an input
inpSpentOutput' :: (DSL.Hash h Addr, Monad m)
                => Input h Addr -> IntT h e m (Output h Addr)
inpSpentOutput' (Input h ix) =  do
    tx <- findHash' h
    case DSL.trOuts tx `at` fromIntegral ix of
      Nothing  -> throwError $ Left $ IntIndexOutOfRange (pretty h) ix
      Just out -> return out

{-------------------------------------------------------------------------------
  Interpreter proper
-------------------------------------------------------------------------------}

data DSL2Abstract

instance Interpretation DSL2Abstract where
  type IntCtx DSL2Abstract = IntT

instance DSL.Hash h Addr => Interpret DSL2Abstract h (DSL.Output h Addr) where
  type Interpreted DSL2Abstract (DSL.Output h Addr) = Output h Addr

  int :: Monad m
      => DSL.Output h Addr
      -> IntT h e m (Output h Addr)
  int out = do
    -- Compute the stake repartition function. At present, this is fixed to
    -- assign all stake to the bootstrap stakeholders.
    return $ Output
      { outAddr = DSL.outAddr out
      , outVal = DSL.outVal out
      , outRepartition = Repartition $ Map.empty
      }

-- | Transactions are interpreted through attaching a list of witnesses.
--
--   We also keep a record of all transactions in the interpretation context.
instance DSL.Hash h Addr => Interpret DSL2Abstract h (DSL.Transaction h Addr) where
  type Interpreted DSL2Abstract (DSL.Transaction h Addr) = Transaction h Addr

  int :: Monad m
      => DSL.Transaction h Addr
      -> IntT h e m (Transaction h Addr)
  int tr = do
      putTx tr
      outs <- mapM (int @DSL2Abstract . nonEmptyEx IntEmptyOutputs) $ DSL.trOuts tr
      ins <- mapM (nonEmptyEx IntEmptyInputs) $ Set.toList $ DSL.trIns tr
      return $ Transaction
        { trFresh = DSL.trFresh tr
        , trIns = ins
        , trOuts = outs
        , trFee = DSL.trFee tr
        , trHash = DSL.trHash tr
        , trExtra = DSL.trExtra tr
        }
    where
      nonEmptyEx :: IntException -> [a] -> IntT h e m (NE a)
      nonEmptyEx ex [] = throwError ex
      nonEmptyEx _ (x:xs) = x :| xs

instance DSL.Hash h Addr => Interpret DSL2Abstract h (DSL.Block h Addr) where
  type Interpreted DSL2Abstract (DSL.Block h Addr) = Block h Addr

  int :: Monad m
      => DSL.Block h Addr
      -> IntT h e m (Block h Addr)
  int block = do
    c :| cs  <- use icCheckpoints
    return $ Block
      { blockPred = icBlockHash c
      , blockSlot = icSlotId c
      , blockIssuer = undefined
      , blockTransactions = mapM (int @DSL2Abstract) block
      , blockDlg = []
      }

instance DSL.Hash h Addr => Interpret DSL2Abstract h (DSL.Chain h Addr) where
  type Interpreted DSL2Abstract (DSL.Chain h Addr) = Chain h Addr

  int :: Monad m
      => DSL.Chain h Addr
      -> IntT h e m (Chain h Addr)
  int = mapM (int @DSL2Abstract)
