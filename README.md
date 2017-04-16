# BMCS Haskell SDK

Haskell SDK for Oracle Bare Metal Cloud Services.

This library provides a Haskell interface for working with the Oracle Bare Metal Cloud

## Credentials

In order to make request to the Oracle Bare Metal Cloud API, you will need credentials.

In most cases these will be loaded from the default profile and location

```haskell
import Network.Oracle.BMC.Credentials(configFileCredentialsProvider, Credentials)

creds :: IO Credentials
creds = configFileCredentialsProvider "~/.oraclebmc/config" "DEFAULT"
```
## Examples

Some examples to get you started

### Instances

Get a single instance by ID

```haskell
import Network.Oracle.BMC.Core.Client
import Network.Oracle.BMC.Core.Requests

-- λ> getInstance defaultCredentialsProvider (getInstancesRequest "ocid...")
-- Right (Instance {availabilityDomain = "NWuj:PHX-AD-1", 
--                  compartmentId = "ocid1.compartment.oc1...", 
--                  displayName = "Some Instance", 
--                  id = "ocid1.instance.oc1.phx....", 
--                  imageId = "ocid1.image.oc1.phx....", 
--                  region = "phx", 
--                  shape = "VM.Standard1.4", 
--                  timeCreated = "2017-01-04T15:50:31.288Z"})
```

## Links and references

* https://tools.ietf.org/html/draft-cavage-http-signatures-05

