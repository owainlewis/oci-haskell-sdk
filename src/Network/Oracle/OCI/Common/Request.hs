{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.OCI.Common.Request
  ( OCIRequest(..)
  , toRequest
  ) where

import           Data.ByteString      as BS
import           Network.HTTP.Simple

import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Client  as H
import qualified Network.HTTP.Types   as H

type Pair = (BS.ByteString, BS.ByteString)

data OCIRequest = OCIRequest {
    host        :: BS.ByteString
  , path        :: BS.ByteString
  , method      :: BS.ByteString
  , body        :: Maybe LBS.ByteString
  , headers     :: [Pair]
  , queryString :: Maybe [Pair]
} deriving ( Eq, Ord, Show )

assignRequestQuery :: Maybe [(ByteString, ByteString)] -> Request -> Request
assignRequestQuery (Just queryString) req =
    setRequestQueryString (Prelude.map (\(a,b) -> (a, Just b)) queryString) req
assignRequestQuery (Nothing) req = req

assignRequestBody :: Maybe LBS.ByteString -> Request -> Request
assignRequestBody (Just bs) req = setRequestBodyLBS bs req
assignRequestBody (Nothing) req = req

toRequest :: OCIRequest -> H.Request
toRequest (OCIRequest host path method hdrs body query) =
    let basicRequest = setRequestHost host
                     $ setRequestPath path
                     $ setRequestMethod method
                     $ setRequestSecure True
                     $ H.defaultRequest
        intermediateRequest = assignRequestQuery query basicRequest in
        intermediateRequest
--        assignRequestBody body intermediateRequest
