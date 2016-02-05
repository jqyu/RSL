module RSL.Utils
  ( class Typeable
  , typeName
  , class Hashable
  , hash
  ) where

class Typeable a where
  typeName :: a -> String

class Hashable a where
  hash :: a -> String
