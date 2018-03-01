{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.OCI.Internal.Query
  ( Path(..)
  , Query(..)
  , unPath
  , unQuery
  , flattenQuery
  , intQueryToByteString
  ) where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as C8

-------------------------------------------------------------------------
--   These two types are used to denote the types of HTTP request params
--   that can be set in the API. I.e query string params vs path params
-------------------------------------------------------------------------
data Path a =
  Path a
  deriving (Eq, Show, Functor)

data Query a =
  Query a
  deriving (Eq, Show, Functor)

-- | Given a query that contains many optional / maybe values, flatten the
--   query returning only the required fields. This is used to build the
--   HTTP request query string
flattenQuery :: [(t, Maybe (Query a))] -> [(t, Maybe a)]
flattenQuery ls = [(k, Just . unQuery $ x) | (k, Just x) <- ls]

-- | Extract the underlying value from a query
--
unQuery :: Query t -> t
unQuery (Query x) = x

-- | Extract the underlying value from a path
--
unPath :: Path t -> t
unPath (Path x) = x

-- | Given a query of Int, transform to a ByteString for dispatch
--
intQueryToByteString :: Query Int -> Query ByteString
intQueryToByteString = fmap (C8.pack . show)
