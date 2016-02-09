module RSL.Core.ResultVar
  where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Ref ( REF, Ref, newRef, readRef )
import Control.Monad.Eff.Exception ( Error )

--------------------------------
-- | Results

data ResultVal a
  = ResultDone a
  | ResultThrow Error
  | ResultBlocked

newtype ResultVar a = ResultVar (Ref (ResultVal a))

newResult :: forall a e. a -> Eff ( ref :: REF | e ) (ResultVar a)
newResult x = ResultVar <$> newRef (ResultDone x)

newEmptyResult :: forall a e. Eff ( ref :: REF | e ) (ResultVar a)
newEmptyResult = ResultVar <$> newRef ResultBlocked

readResult :: forall a e. ResultVar a -> Eff ( ref :: REF | e ) (ResultVal a)
readResult (ResultVar a) = readRef a
