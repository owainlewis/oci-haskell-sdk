module Network.Oracle.BMC.RequestBase
  ( Path(..)
  , Query(..)
  , unQuery
  , unPath
  , ToRequest(..)
  ) where

import Network.HTTP.Client (Request)

data Path a =
  Path a
  deriving (Eq, Show)

data Query a =
  Query a
  deriving (Eq, Show)

class ToRequest a where
  toRequest :: a -> Request

-- | Extract the underlying value from a query
--
unQuery :: Query t -> t
unQuery (Query x) = x

-- | Extract the underlying value from a path
--
unPath :: Path t -> t
unPath (Path x) = x
