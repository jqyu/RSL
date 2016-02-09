module Services.Radredis where

import Prelude

import Data.Exists
import Data.List

import RSL.Core
import RSL.Utils

type OType = String
type Id = Int

data RRRequest a
  = GetIndex  OType
  | GetObject OType Id
  | IncrId    OType
  | Save      OType Id
  | Delete    OType Id

dispatch :: Flags
         -> List (Exists (BlockedFetch RRRequest))
         -> PerformFetch
dispatch _ reqs = AsyncFetch do
  return unit

instance showRadredisRequest :: Show (RRRequest a) where
  show (GetIndex t)     = "GET INDEX FOR :: " ++ t
  show (GetObject t id) = "GET OBJECT    :: " ++ t ++ ":" ++ show id
  show (IncrId t)       = "INCR ID FOR   :: " ++ t
  show (Save t id)      = "SAVE OBJECT   :: " ++ t ++ ":" ++ show id
  show (Delete t id)    = "DELETE OBJECT :: " ++ t ++ ":" ++ show id

instance hashEchoRequest :: Hashable (RRRequest a) where
  hash (GetIndex t)     = "radredis:getindex:" ++ t
  hash (GetObject t id) = "radredis:get:"      ++ t ++ ":" ++ show id
  hash (IncrId t)       = "radredis:incrid:"   ++ t
  hash (Save t id)      = "radredis:save:"     ++ t ++ ":" ++ show id
  hash (Delete t id)    = "radredis:delete:"   ++ t

instance requestRadredis :: Request RRRequest a where
  fetcher r = new (key r) dispatch
  key     _ = "radredis"
