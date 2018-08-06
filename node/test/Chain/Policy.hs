-- | Chain policies represent validation properties plus generators to construct
--   both validating and non-validating chains.
module Chain.Policy where

import Chain.Abstract
import Chain.Validation
import qualified UTxO.DSL as DSL
import Universum

-- | Control whether to construct valid or invalid blocks.
data GenValidity
  = Valid
  | Invalid

-- | A block modifier.
newtype BlockModifier genM h a = BlockModifier
  { modifyBlock :: Chain h a -> Block h a -> genM (Block h a) }

instance Monad genM => Monoid (BlockModifier genM h a) where
  mempty = BlockModifier $ const return
  (BlockModifier f1) `mappend` (BlockModifier f2) = BlockModifier $ \ch bl ->
      f1 ch bl >>= f2 ch

-- | A 'Policy' should correspond to a particular aspect of the system we want
-- to test. It provides both the means to validate a chain extension against the
-- policy, and to generate valid or valid blocks.
data Policy genM valE = Policy
  { polValidation :: Chain DSL.IdentityAsHash Addr -> Validation valE ()
  , polGenerator :: GenValidity -> BlockModifier genM DSL.IdentityAsHash Addr
  }
