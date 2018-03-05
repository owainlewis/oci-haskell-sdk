{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.OCI where

import           Network.Oracle.OCI.Common.Signer

import qualified Network.HTTP.Client                     as H
import           Network.HTTP.Simple
import qualified Network.HTTP.Types                      as H
import qualified Data.ByteString.Char8                   as C8

key = "ocid1.tenancy.oc1..aaaaaaaaxf3fuazosc6xng7l75rj6uist5jb6ken64t3qltimxnkymddqbma/ocid1.user.oc1..aaaaaaaa3p67n2kmpxnbcnffjow6j5bhe6jze3obob3cjdctfftyfd4zou2q/a4:bb:34:43:54:c5:af:a5:4b:23:ce:82:2d:7f:12:45"

signAndDispatchRequest req = do
  req' <- signRequest req key
  httpLBS req'

doRequest :: H.Request -> IO Int
doRequest request = do
  response <- httpLBS request
  return $ getResponseStatusCode response

demoRequest :: H.Request
demoRequest =
        setRequestHost "identity.us-ashburn-1.oraclecloud.com"
      $ setRequestPath "/20160918/compartments"
      $ setRequestSecure True
      $ setRequestPort 443
      $ setRequestQueryString [("compartmentId", Just "ocid1.tenancy.oc1..aaaaaaaaxf3fuazosc6xng7l75rj6uist5jb6ken64t3qltimxnkymddqbma")]
      $ H.defaultRequest
