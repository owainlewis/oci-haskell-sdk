{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Transport.Request where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import qualified Data.ByteString.Lazy as LBS

import qualified Data.CaseInsensitive as CI

base = "https://iaas.us-phoenix-1.oraclecloud.com"

-- A simple abstraction for HTTP requests to make it easier to test and verify HTTP inputs
data OracleRequest = OracleRequest { url :: String
                                   , requestMethod :: String
                                   , headers :: [(BS.ByteString, BS.ByteString)]
                                   , body :: Maybe BS.ByteString
                                   } deriving ( Eq, Ord, Read, Show )

requiredHeaders = ["date", "(request-target)", "host"]

transformRequest :: OracleRequest -> IO Network.HTTP.Client.Request
transformRequest request = do
      manager <- newManager tlsManagerSettings
      initialRequest <- parseRequest (url request)
      let req = initialRequest
                    { method = C8.pack (requestMethod request)
                    , requestHeaders = map (\(k, v) -> (CI.mk k, v)) (headers request)
                    }
      return $ maybe req (\lbs -> req { requestBody = RequestBodyBS lbs }) (body request)

runHttpsRequest :: OracleRequest -> IO (Network.HTTP.Client.Response LBS.ByteString)
runHttpsRequest req = do
    manager <- newManager tlsManagerSettings
    internalRequest <- transformRequest req
    response <- httpLbs internalRequest manager
    return response
