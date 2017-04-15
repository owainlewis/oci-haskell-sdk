# BMCS Haskell SDK (WIP)

Haskell SDK for Oracle Bare Metal Cloud Services.

This library provides a Haskell interface for working with the Oracle Bare Metal Cloud

## Credentials

In order to make request to the Oracle Bare Metal Cloud API, you will need credentials. (see TODO)

```haskell

import qualified Network.Oracle.BMC.Credentails as Credentials

creds :: IO Credentials
creds = Credentials.configFileCredentialsProvider "~/.oraclebmc/config" "DEFAULT"
```

## Examples

### Instances

Get a list of all instances in a compartment

```

```

Get a single instance

```
Client.getInstance (getInstanceRequest "ocid...")
```

configFileCredentialsProvider "~/.oraclebmc/config" "DEFAULT"

## Links and references

* https://tools.ietf.org/html/draft-cavage-http-signatures-05

