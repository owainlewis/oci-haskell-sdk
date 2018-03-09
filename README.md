# OCI Haskell SDK

### Haskell SDK for Oracle Cloud Infrastructure

This library provides a Haskell interface for working with the Oracle Bare Metal Cloud

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.OCI where

import qualified Network.Oracle.OCI.Common.Client      as Client
import           Network.Oracle.OCI.Common.Credentials (readCredentialsFromFile)

import qualified Data.ByteString.Char8                 as C8
import qualified Data.ByteString.Lazy                  as LBS
import qualified Network.HTTP.Client                   as H
import           Network.HTTP.Simple
import qualified Network.HTTP.Types                    as H

demoRequest :: H.Request
demoRequest =
		setRequestHost "identity.us-ashburn-1.oraclecloud.com"
	  $ setRequestPath "/20160918/compartments"
	  $ setRequestSecure True
	  $ setRequestPort 443
	  $ setRequestQueryString [("compartmentId", Just "ocid1.tenancy.oc1..aaaaaaaaxf3fuazosc6xng7l75rj6uist5jb6ken64t3qltimxnkymddqbma")]
	  $ H.defaultRequest

main :: IO (Response LBS.ByteString)
main = do
  credentials <- readCredentialsFromFile "/Users/owainlewis/.oci/config" "DEFAULT"
  Client.requestLBS credentials demoRequest
```

## Links and references

* https://tools.ietf.org/html/draft-cavage-http-signatures-05
