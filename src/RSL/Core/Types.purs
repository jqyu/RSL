module RSL.Core.Types
  ( Flags(..)
  , defaultFlags

  , Stats(..)
  , Microseconds
  , RoundStats(..)
  , emptyStats

  , Request
  , DataSource
  , PerformFetch(..)
  , fetch

  , ResultVar(..)
  , BlockedFetch(..)

  ) where

import Prelude

import Control.Monad.Aff
import Control.Monad.Aff.AVar
import Control.Monad.Eff.Exception
  ( Error(..)
  )

import Data.Either ( Either(..) )
import Data.List ( List(..) )

import RSL.Utils
  ( Typeable
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
  = SyncFetch (forall e. Aff e Unit)
  | AsyncFetch (forall e. Aff e Unit)

class DataSource req where
  fetch :: Flags -> List forall a. (BlockedFetch req a) -> PerformFetch

class ( Show (req a)
      , Typeable (req a)
      ) <= Request req a

newtype ResultVar a = ResultVar (AVar (Either Error a))

data BlockedFetch r a = BlockedFetch (r a) (ResultVar a)

instance typeOfBlockedFetch :: (Request r a) => Typeable (BlockedFetch r a) where
  typeName (BlockedFetch req _) = typeName req
