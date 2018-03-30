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
  , body        :: Maybe BS.ByteString
  , headers     :: [Pair]
  , queryString :: Maybe [Pair]
} deriving ( Eq, Ord, Show )

--addQueryString = -- $ setRequestQueryString [("compartmentId", Just "ocid1.tenancy.oc1..aaaaaaaaxf3fuazosc6xng7l75rj6uist5jb6ken64t3qltimxnkymddqbma")]

toRequest :: OCIRequest -> H.Request
toRequest (OCIRequest host path method hdrs body query) =
    let basicRequest = setRequestHost host
                     $ setRequestPath path
                     $ setRequestMethod method
                     $ setRequestSecure True
                     $ H.defaultRequest in
      basicRequest
