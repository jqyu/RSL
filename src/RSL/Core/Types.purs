module RSL.Core.Types
  ( Flags(..)
  , defaultFlags

  , Stats(..)
  , Microseconds
  , RoundStats(..)
  , emptyStats

  , class Request
  , class DataSource
  , PerformFetch(..)
  , fetch

  , ResultVar(..)
  , ResultVal(..)
  , newResult
  , newEmptyResult
  , readResult
  , BlockedFetch(..)

  ) where

import Prelude

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Console ( CONSOLE )
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Exception
  ( Error(..)
  )

import Data.Either ( Either(..) )
import Data.List ( List(..) )

import RSL.Utils
  ( class Hashable
  , class Typeable
  , typeName
  )

--------------------------------
-- | Debugging Types and Tools

newtype Flags = Flags
  { trace :: Int
  -- ^ Tracing level, (0 = quiet)
  , report :: Int
  -- ^ Reporting level, (0 = quiet, 1 = # of requests, 2 = time, 3 = # of errors)
  }
defaultFlags :: Flags
defaultFlags = Flags
  { trace: 0
  , report: 1
  }
instance showFlags :: Show Flags where
  show (Flags f) =
    "|> FLAGS =========\n" ++
    "| Trace flag: " ++ show f.trace ++ "\n" ++
    "| Report flag:" ++ show f.report ++ "\n" ++
    "| ----------------"


type Microseconds = Int

newtype Stats = Stats (List RoundStats) -- add more as necessary
emptyStats :: Stats
emptyStats = Stats Nil

data RoundStats = RoundStats
  { roundTime :: Microseconds
  -- add more fields as you need them
  }


--------------------------------
-- | Data Source Classes

data PerformFetch
  = SyncFetch (forall e. Aff ( console :: CONSOLE | e ) Unit)
  | AsyncFetch (  (forall e. Aff ( console :: CONSOLE | e ) Unit)
               -> (forall e. Aff ( console :: CONSOLE | e ) Unit)
               )

class DataSource req where
  fetch :: Flags -> List (forall a. (BlockedFetch req a)) -> PerformFetch

class ( Show (req a)
      , Typeable (req a)
      , Hashable (req a)
      , DataSource req
      ) <= Request req a

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

-- really wish the magic of deriving was here
data BlockedFetch r a = BlockedFetch (r a) (ResultVar a)
instance typeOfBlockedFetch :: (Request r a)
                            => Typeable (BlockedFetch r a) where
  typeName (BlockedFetch req _) = typeName req
