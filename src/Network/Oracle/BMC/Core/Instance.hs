{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Core.Instance where

import           Network.Oracle.BMC.Transport.Request

import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Char8                as C

--------------------------------------------------------

base :: String
base = "https://iaas.us-phoenix-1.oraclecloud.com"

version :: String
version = "20160918"

endpoint :: String
endpoint = base ++ "/" ++ version

--------------------------------------------------------

listInstances :: BS.ByteString -> IO HttpResponse
listInstances compartmentId =
    let req = HttpRequest { httpMethod = GET
                          , url = endpoint ++ "/instances"
                          , headers = []
                          , body = Nothing
                          , query = Just [("compartmentId", compartmentId)]
                          } in
    runHttpsRequest req
