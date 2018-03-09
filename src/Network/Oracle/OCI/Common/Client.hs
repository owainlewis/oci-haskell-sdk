{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.OCI.Common.Client
  ( requestLBS
  ) where

import           Network.Oracle.OCI.Common.Credentials       (Credentials)
import           Network.Oracle.OCI.Common.Signatures.Signer (signRequest)

import qualified Data.ByteString.Char8                       as C8
import qualified Data.ByteString.Lazy                        as LBS
import qualified Network.HTTP.Client                         as H
import           Network.HTTP.Simple
import qualified Network.HTTP.Types                          as H

newtype SignClient = SignClient {
  requestSigner :: Credentials -> H.Request -> IO H.Request
}

requestLBS :: Credentials -> H.Request -> IO (Response LBS.ByteString)
requestLBS credentials request = signRequest credentials request >>= httpLBS
