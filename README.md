# OCI Haskell SDK

### Haskell SDK for Oracle Cloud Infrastructure

This library provides a Haskell interface for working with the Oracle Bare Metal Cloud

## Notes

Signature 

```
echo "Hello, World" | openssl dgst -sha256 -sign oci_api_key.pem | openssl enc -e -base64 | tr -d '\n'
```

### Supported services

* Compute
* Object Storage

## Credentials

In order to make request to the Oracle Bare Metal Cloud API, you will need credentials.

In most cases these will be loaded from the default profile and location

```haskell
import Network.Oracle.BMC.Credentials(configFileCredentialsProvider, Credentials)

creds :: IO Credentials
creds = configFileCredentialsProvider "~/.oraclebmc/config" "DEFAULT"
```

## Introduction

This library offers high level APIs for working with the Oracle Bare Metal Cloud Service. All responses
have the following signature. 

```haskell
type BMCAPIResponse a = IO (Either APIError a)
```

In the case of a non 200 repsonse code the library will cast the underlying
repsonse into an appropritate type.

```haskell
Left (APIError {code = "MissingParameter", message = "Missing compartmentId"})
```

## Examples

Some examples to get you started

### Instances

Get a single instance by ID

```haskell
import Network.Oracle.BMC.Core.Client
import Network.Oracle.BMC.Core.Requests

response :: BMCSAPIResponse
response = getInstance defaultCredentialsProvider (getInstancesRequest "ocid...")

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

