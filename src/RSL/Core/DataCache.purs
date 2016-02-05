module RSL.Core.DataCache
  ( DataCache(..)
  , empty
  , insert
  , lookup
  ) where

import Prelude

import Data.Maybe ( Maybe(..) )
import Data.StrMap ( StrMap(..) )
import Data.StrMap as StrMap

import Data.Exists
  ( Exists(..)
  , mkExists
  , runExists
  )

import Unsafe.Coerce ( unsafeCoerce )
import Unsafe.Coerce ( unsafeCoerce )

import RSL.Core.Types
  ( class DataSource
  , class Request
  , ResultVar(..)
  )

import RSL.Utils
  ( hash
  )

newtype DataCache = DataCache (StrMap (Exists ResultVar))

empty :: DataCache
empty = DataCache $ StrMap.empty

insert :: forall req a. (Request req a)
       => req a
       -> ResultVar a
       -> DataCache
       -> DataCache
insert req res (DataCache m) =
    DataCache $ StrMap.insert key res' m
  where key  = hash req
        res' = mkExists res

lookup :: forall req a. (Request req a)
       => req a
       -> DataCache
       -> Maybe (ResultVar a)
lookup req (DataCache m) =
  runExists unsafeCoerce <$> StrMap.lookup key m
  where key = hash req
