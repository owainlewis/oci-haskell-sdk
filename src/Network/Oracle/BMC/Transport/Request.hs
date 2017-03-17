{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Transport.Request where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)

base = "https://iaas.us-phoenix-1.oraclecloud.com/20160918/vcns"

-- Core Services API: https://iaas.us-phoenix-1.oraclecloud.com (covering the Networking Service, Compute Service, and Block Volume Service)
-- Load Balancing Service API: https://iaas.us-phoenix-1.oraclecloud.com
-- IAM Service API: https://identity.us-phoenix-1.oraclecloud.com
-- Object Storage Service API: https://objectstorage.us-phoenix-1.oraclecloud.com
-- Database Service API: https://database.us-phoenix-1.oraclecloud.com
-- Audit Service API: https://audit.us-phoenix-1.oraclecloud.com


runRequest method url = do
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest url
    let request = initialRequest { method = method }
    response <- httpLbs request manager
    return response



