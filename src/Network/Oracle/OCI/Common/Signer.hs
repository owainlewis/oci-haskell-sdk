{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
--- |
--- Module      :  Network.Oracle.OCI.Common.Signer
--- License     :  BSD-style (see the file LICENSE)
---
--- Maintainer  :  Owain Lewis <owain.lewis@oracle.com>
---
--- This module will perform the signing of all HTTP requests.
---
--- For more information see:
---   1. https://tools.ietf.org/html/draft-cavage-http-signatures-08
---   2. https://docs.us-phoenix-1.oraclecloud.com/Content/API/Concepts/signingrequests.htm
---
--- In general, these are the steps required to sign a request:
---   1. Form the HTTPS request (SSL protocol TLS 1.2 is required).
---   2. Create the signing string, which is based on parts of the request.
---   3. Create the signature from the signing string, using your private key and the RSA-SHA256 algorithm.
---   4. Add the resulting signature and other required information to the Authorization header in the request.
------------------------------------------------------------------------------
module Network.Oracle.OCI.Common.Signer
  ( signRequest
  , computeSignature
  ) where

import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Char8                   as C8
import           Data.CaseInsensitive                    (original)
import           Data.Char                               (toLower)
import           Data.Monoid                             ((<>))
import qualified Data.Text                               as T
import           Data.Time
import qualified Network.HTTP.Client                     as H
import           Network.HTTP.Simple
import qualified Network.HTTP.Types                      as H

import           Network.Oracle.OCI.Common.Configuration (Configuration)
import qualified Network.Oracle.OCI.Common.OpenSSL       as OpenSSL

type HeaderTransformer = H.Request -> IO H.Request

defaultGenericHeaders :: [BS.ByteString]
defaultGenericHeaders = ["date", "(request-target)", "host"]

defaultBodyHeaders :: [BS.ByteString]
defaultBodyHeaders = ["content-length", "content-type", "x-content-sha256"]

-- | Add a request target header to a request. This is needed by all requests
--
addRequestTargetHeader :: HeaderTransformer
addRequestTargetHeader request =
  pure $ setRequestHeader "(request-target)" [target] request
  where
    rMethod = lowerCaseBS (H.method request)
    lowerCaseBS = C8.pack . map toLower . C8.unpack
    target = rMethod <> " " <> (H.path request) <> (H.queryString request)

addHostHeader :: HeaderTransformer
addHostHeader request = pure (setRequestHeader "host" [H.host request] request)

addDateHeader :: HeaderTransformer
addDateHeader request = do
  now <-
    C8.pack <$> formatTime defaultTimeLocale "%a, %d %h %Y %H:%M:%S GMT" <$>
    getCurrentTime
  return $ setRequestHeader "date" [now] request

addDefaultHeaders :: Request -> IO Request
addDefaultHeaders request = addDateHeader request >>= addHostHeader >>= addRequestTargetHeader

computeSignature :: Request -> BS.ByteString
computeSignature request = BS.intercalate "\n" hdrs
  where
    hdrs = map (\(k, v) -> original k <> ": " <> v) (H.requestHeaders request)

base64EncodedRequestSignature :: Request -> IO BS.ByteString
base64EncodedRequestSignature request = do
  let signature = computeSignature request
  OpenSSL.signWithPrivateKey "/Users/owainlewis/.oci/oci_api_key.pem" (C8.unpack signature)

zipHeaderPairs :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
zipHeaderPairs pairs = BS.intercalate "," $ map (\(k, v) -> k <> "=\"" <> v <> "\"") pairs

generateSigningString :: Request -> BS.ByteString -> IO BS.ByteString
generateSigningString request keyId =
  let headers = (BS.intercalate " ") . map (original . fst) . H.requestHeaders in do
  signature <- base64EncodedRequestSignature request
  return $ zipHeaderPairs [ ("headers", headers request)
                          , ("keyId", keyId)
                          , ("algorithm", "rsa-sha256")
                          , ("signature", signature)
                          ]

addAuthHeader :: Request -> BS.ByteString -> IO Request
addAuthHeader request keyId =
  let headers = (BS.intercalate " ") . map (original . fst) . H.requestHeaders in do
  signature <- base64EncodedRequestSignature request
  let requestSignature = zipHeaderPairs [ ("headers", headers request)
                                        , ("keyId", keyId)
                                        , ("algorithm", "rsa-sha256")
                                        , ("signature", signature)
                                        ]
  return $ setRequestHeader "authorization" ["Signature " <> requestSignature] request

key = "ocid1.tenancy.oc1..aaaaaaaaxf3fuazosc6xng7l75rj6uist5jb6ken64t3qltimxnkymddqbma/ocid1.user.oc1..aaaaaaaa3p67n2kmpxnbcnffjow6j5bhe6jze3obob3cjdctfftyfd4zou2q/a4:bb:34:43:54:c5:af:a5:4b:23:ce:82:2d:7f:12:45"

signRequest :: Request -> IO Request
signRequest req = do
  req' <- addDefaultHeaders req >>= (flip addAuthHeader key)
  return req'

signAndDispatchRequest req = do
  req' <- signRequest req
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
