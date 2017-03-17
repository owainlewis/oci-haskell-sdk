{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Transport.Request where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)

base = "https://iaas.us-phoenix-1.oraclecloud.com/20160918/vcns"

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest base
    let request = initialRequest { method = "GET" }
    response <- httpLbs request manager
    putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)



