module Network.Oracle.BMC.SignatureSpec where

import qualified Network.Oracle.BMC.Signature as Signature
import qualified Data.ByteString.Char8 as C

publicKey :: String
publicKey = unlines
  [ "-----BEGIN PUBLIC KEY-----"
  , "MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDCFENGw33yGihy92pDjZQhl0C3"
  , "6rPJj+CvfSC8+q28hxA161QFNUd13wuCTUcq0Qd2qsBe/2hFyc2DCJJg0h1L78+6"
  , "Z4UMR7EOcpUE9Hf3m/hs+FUR45uBJeDK1HSFHD8bHKD6kv8FPGfJTotc+2xjJw"
  , "oYi+1hqp1fIekaxsyQIDAQAB"
  , "-----END PUBLIC KEY-----"
  ]

privateKey :: String
privateKey = unlines
  [ "-----BEGIN RSA PRIVATE KEY-----"
  , "MIICXgIBAAKBgQDCFENGw33yGihy92pDjZQhl0C36rPJj+CvfSC8+q28hxA161QF"
  , "NUd13wuCTUcq0Qd2qsBe/2hFyc2DCJJg0h1L78+6Z4UMR7EOcpUE9Hf3m/hs+F"
  , "UR45uBJeDK1HSFHD8bHKD6kv8FPGfJTotc+2xjJwoYi+1hqp1fIekaxsyQIDAQAB"
  , "AoGBAJR8ZkCUvx5kzv+utdl7T5MnordT1TvoXXJGXK7ZZ+UuvMNUCdN2QPc4sBiA"
  , "QWvLw1cSKt5DsKZ8UETpYPy8pPYnnDEz2dDYiaew9+xEpubyeW2oH4Zx71wqBtOK"
  , "kqwrXa/pzdpiucRRjk6vE6YY7EBBs/g7uanVpGibOVAEsqH1AkEA7DkjVH28WDUg"
  , "f1nqvfn2Kj6CT7nIcE3jGJsZZ7zlZmBmHFDONMLUrXR/Zm3pR5m0tCmBqa5RK95u"
  , "412jt1dPIwJBANJT3v8pnkth48bQo/fKel6uEYyboRtA5/uHuHkZ6FQF7OUkGogc"
  , "mSJluOdc5t6hI1VsLn0QZEjQZMEOWr+wKSMCQQCC4kXJEsHAve77oP6HtG/IiEn7"
  , "kpyUXRNvFsDE0czpJJBvL/aRFUJxuRK91jhjC68sA7NsKMGg5OXb5I5Jj36xAkEA"
  , "gIT7aFOYBFwGgQAQkWNKLvySgKbAZRTeLBacpHMuQdl1DntvAyqpAZ0lY0RKmW"
  , "G6aFKaqQfOXKCyWoUiVknQJAXrlgySFci/2ueKlIE1QqIiLSZ8V8OlpFLRnb1pzI"
  , "7U1yQXnTAEFYM560yJlzUpOb1V4cScGd365tiSMvxLOvTA=="
  , "-----END RSA PRIVATE KEY-----"
  ]

signingString = unlines
  [ "date: Thu, 05 Jan 2014 21:31:40 GMT"
  , "(request-target): get /20160918/instances?availabilityDomain=Pjwf%3A%20PHX-AD-1&compartmentId=ocid1.compartment.oc1..aaaaaaaam3we6vgnherjq5q2idnccdflvjsnog7mlr6rtdb25gilchfeyjxa&displayName=TeamXInstances&volumeId=ocid1.volume.oc1.phx.abyhqljrgvttnlx73nmrwfaux7kcvzfs3s66izvxf2h4lgvyndsdsnoiwr5q"
  , "host: iaas.us-phoenix-1.oraclecloud.com"
  ]

-- For the following GET request (line breaks inserted between query parameters for easier reading; also notice the URL encoding as mentioned earlier):

-- GET https://iaas.us-phoenix-1.oraclecloud.com/20160918/instances
-- ?availabilityDomain=Pjwf%3A%20PHX-AD-1
-- &compartmentId=ocid1.compartment.oc1..aaaaaaaam3we6vgnherjq5q2idnccdflvjsnog7mlr6rtdb25gilchfeyjxa
-- &displayName=TeamXInstances
-- &volumeId=ocid1.volume.oc1.phx.abyhqljrgvttnlx73nmrwfaux7kcvzfs3s66izvxf2h4lgvyndsdsnoiwr5q
-- Date: Thu, 05 Jan 2014 21:31:40 GMT

--   The signing string would be (line breaks inserted into the (request-target) header for easier reading):

--   date: Thu, 05 Jan 2014 21:31:40 GMT
--   (request-target): get /20160918/instances?availabilityDomain=Pjwf%3A%20PH
--   X-AD-1&compartmentId=ocid1.compartment.oc1..aaaaaaaam3we6vgnherjq5q2i
--   dnccdflvjsnog7mlr6rtdb25gilchfeyjxa&displayName=TeamXInstances&
--   volumeId=ocid1.volume.oc1.phx.abyhqljrgvttnlx73nmrwfaux7kcvzfs3s66izvxf2h
--   4lgvyndsdsnoiwr5q
--   host: iaas.us-phoenix-1.oraclecloud.com

--   The Authorization header would be:
--   Signature version="1",headers="date (request-target) host",keyId="ocid1.t
--   enancy.oc1..aaaaaaaaba3pv6wkcr4jqae5f15p2b2m2yt2j6rx32uzr4h25vqstifssq/
--   ocid1.user.oc1..aaaaaaaat5nvwcna5j6aqzjcaty5eqbb6qt2jvpkanghtgdaqedqw3ryn
--   jq/20:3b:97:13:55:1c:5b:0d:d3:37:d8:50:4e:c5:3a:34",algorithm="rsa-sha256
--   ",signature="GBas7grhyrhSKHP6AVIj/h5/Vp8bd/peM79H9Wv8kjoaCivujVXlpbKLjMPe
--   DUhxkFIWtTtLBj3sUzaFj34XE6YZAHc9r2DmE4pMwOAy/kiITcZxa1oHPOeRheC0jP2dqbTll
--   8fmTZVwKZOKHYPtrLJIJQHJjNvxFWeHQjMaR7M="


key = Signature.sign "test/Fixtures/bmcs_api_key.pem" (C.pack "Hello, World")
