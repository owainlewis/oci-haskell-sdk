{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Transport.Request where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)

base = "https://iaas.us-phoenix-1.oraclecloud.com"

requiredHeaders = ["date" "(request-target)" "host"]

runRequest method url = do
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest url
    let request = initialRequest { method = method }
    response <- httpLbs request manager
    return response



