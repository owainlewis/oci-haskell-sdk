{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.OCI.Common.Client
  ( runRequest
  , runRequestRaw
  , APIResponse
  ) where

import           Network.Oracle.OCI.Common.Api               (APIError)
import           Network.Oracle.OCI.Common.Credentials       (Credentials)
import           Network.Oracle.OCI.Common.Signatures.Signer (signRequest)

import           Data.Aeson                                  (decode)
import qualified Data.ByteString.Char8                       as C8
import qualified Data.ByteString.Lazy                        as LBS
import qualified Network.HTTP.Client                         as H
import           Network.HTTP.Simple
import qualified Network.HTTP.Types                          as H

type APIResponse = Either (Maybe APIError) LBS.ByteString

newtype SignClient = SignClient {
  requestSigner :: Credentials -> H.Request -> IO H.Request
}

runRequestRaw :: Credentials -> H.Request -> IO (Response LBS.ByteString)
runRequestRaw credentials request = signRequest credentials request >>= httpLBS

runRequest :: Credentials -> H.Request -> IO APIResponse
runRequest credentials request = do
  response <- (signRequest credentials request >>= httpLBS)
  let responseCode = H.statusCode $ H.responseStatus response
  if responseCode >= 400 then return . Left $ decode (H.responseBody response)
                         else return . Right $ H.responseBody response
