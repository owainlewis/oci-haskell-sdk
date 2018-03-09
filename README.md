# OCI Haskell SDK

### Haskell SDK for Oracle Cloud Infrastructure

This library provides a Haskell interface for working with the Oracle Bare Metal Cloud

```haskell
module Network.Oracle.OCI where

import           Network.Oracle.OCI.Common.Credentials (readCredentialsFromFile)
import           Network.Oracle.OCI.Common.Signer

import qualified Data.ByteString.Char8                 as C8
import qualified Network.HTTP.Client                   as H
import           Network.HTTP.Simple
import qualified Network.HTTP.Types                    as H

demoRequest :: H.Request
demoRequest =
        setRequestHost "identity.us-ashburn-1.oraclecloud.com"
      $ setRequestPath "/20160918/compartments"
      $ setRequestSecure True
      $ setRequestPort 443
      $ setRequestQueryString [("compartmentId", Just "ocid1.tenancy.oc1..")]
      $ H.defaultRequest

main = do
  credentials <- readCredentialsFromFile "/Users/owainlewis/.oci/config" "DEFAULT"
  signRequest credentials demoRequest >>= httpLBS
```

## Links and references

* https://tools.ietf.org/html/draft-cavage-http-signatures-05

