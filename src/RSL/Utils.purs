module RSL.Utils
  ( Typeable
  , typeName
  ) where

class Typeable a where
  typeName :: a -> String
