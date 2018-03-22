{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
--- |
--- Module      :  Network.Oracle.OCI.Common.Signatures.Signer
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
module Network.Oracle.OCI.Common.Signatures.Signer
  ( signRequest
  , computeSignature
  ) where

import           Crypto.Hash                                  (Digest,
                                                               SHA256 (..),
                                                               hash)
import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Char8                        as C8
import           Data.CaseInsensitive                         (original)
import           Data.Char                                    (toLower)
import           Data.Monoid                                  ((<>))
import qualified Data.Text                                    as T
import           Data.Text.Encoding                           (encodeUtf8)
import           Data.Time
import qualified Network.HTTP.Client                          as H
import           Network.HTTP.Simple
import qualified Network.HTTP.Types                           as H
import           Network.Oracle.OCI.Common.Credentials        (Credentials (..),
                                                               getKeyID)
import qualified Network.Oracle.OCI.Common.Signatures.OpenSSL as OpenSSL

type HeaderTransformer = H.Request -> IO H.Request

defaultGenericHeaders :: [BS.ByteString]
defaultGenericHeaders = ["date", "(request-target)", "host"]

defaultBodyHeaders :: [BS.ByteString]
defaultBodyHeaders = ["content-length", "content-type", "x-content-sha256"]

getDigest :: BS.ByteString -> Digest SHA256
getDigest bs = hash bs

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
addDefaultHeaders request =
      addDateHeader request
  >>= addHostHeader
  >>= addRequestTargetHeader

addAllHeaders :: Request -> IO Request
addAllHeaders request = addDefaultHeaders request

-- | TODO filter these by the set of known hdrs above
--
computeSignature :: Request -> String
computeSignature request = C8.unpack $ BS.intercalate "\n" hdrs
  where
    mandatoryHeaders = defaultGenericHeaders ++ defaultBodyHeaders
    filteredHeaders = filter (\(k, v) -> original k `elem` mandatoryHeaders) (H.requestHeaders request)
    hdrs = map (\(k, v) -> original k <> ": " <> v) filteredHeaders

encodeAndSignRequest :: FilePath -> Request -> IO BS.ByteString
encodeAndSignRequest keyPath request = do
  let signature = computeSignature request
  OpenSSL.signWithPrivateKey keyPath signature

addAuthHeader :: Credentials -> Request -> IO Request
addAuthHeader credentials request =
  let headers = (BS.intercalate " ") . map (original . fst) . H.requestHeaders in do
  signature <- encodeAndSignRequest (T.unpack $ keyFile credentials) request
  let key = encodeUtf8 (getKeyID credentials)
      requestSignature = zipHeaderPairs [ ("headers", headers request)
                                        , ("keyId", key)
                                        , ("algorithm", "rsa-sha256")
                                        , ("signature", signature)
                                        ]
  return $ setRequestHeader "authorization" ["Signature " <> requestSignature] request

zipHeaderPairs :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
zipHeaderPairs pairs = BS.intercalate "," $ map (\(k, v) -> k <> "=\"" <> v <> "\"") pairs

signRequest :: Credentials -> Request -> IO Request
signRequest credentials req =
  if (H.method req) == "GET" ||
     (H.method req) == "DELETE"
  then addDefaultHeaders req >>= addAuthHeader credentials
  else addAllHeaders req >>= addAuthHeader credentials
