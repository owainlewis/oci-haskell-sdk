{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Transport.Request
  ( addGenericHeaders
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.CaseInsensitive (original)
import Data.Char (toLower)
import Data.Semigroup ((<>))
import Data.Time
import Network.HTTP.Client (Request(..))
import Network.HTTP.Simple
import qualified Network.Oracle.BMC.Credentials as Credentials
import qualified Network.Oracle.BMC.Signature as Signature

import Control.Monad (forM_)

--------------------------------------------------------------------------------
-- Add HTTP headers
--
-- These methods add the required HTTP headers to a request.
--
--------------------------------------------------------------------------------
type HeaderTransformer = Request -> IO Request

genericHeaders = ["date", "(request-target)", "host"]

addDateHeader :: HeaderTransformer
addDateHeader request = do
  now <-
    C8.pack <$> formatTime defaultTimeLocale "%a, %_d %b %Y %H:%M:%S GMT" <$>
    getCurrentTime
  return $ setRequestHeader "date" [now] request

addRequestTargetHeader :: HeaderTransformer
addRequestTargetHeader request =
  pure $ setRequestHeader "(request-target)" [target] request
  where
    rMethod = lowerCaseBS (method request)
    lowerCaseBS = C8.pack . map toLower . C8.unpack
    target = rMethod <> " " <> (path request) <> (queryString request)

addHostHeader :: HeaderTransformer
addHostHeader request = pure $ setRequestHeader "host" [(host request)] request

addContentTypeDefault :: HeaderTransformer
addContentTypeDefault request =
  pure $ setRequestHeader "content-type" ["application/json"] request

--------------------------------------------------------------------------------
-- | TODO dispatch on HTTP method adding additional headers if needed
addGenericHeaders :: Request -> IO Request
addGenericHeaders request =
  addDateHeader request >>= addRequestTargetHeader >>= addHostHeader

--------------------------------------------------------------------------------
computeSignature :: Request -> BS.ByteString
computeSignature request = BS.intercalate "\n" hdrs
  where
    hdrs = map (\(k, v) -> original k <> ": " <> v) (requestHeaders request)

base64EncodedRequestSignature :: Request -> IO BS.ByteString
base64EncodedRequestSignature request = do
  let signature = computeSignature request
  print signature
  Signature.signBase64Unsafe
    "/home/owainlewis/.oraclebmc/bmcs_api_key.pem" signature

zipHeaderPairs :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
zipHeaderPairs pairs = BS.intercalate "," $ map f pairs
  where
    f = (\(k, v) -> k <> "=\"" <> v <> "\"")

--------------------------------------------------------------------------------
-- | Adds the Authorization Signature HTTP header to a request which is
--   formed by signing the HTTP headers
--
addAuthHeader :: Request -> BS.ByteString -> IO Request
addAuthHeader request keyId =
  let headers _ = "date (request-target) host"
  in do signature <- base64EncodedRequestSignature request
        let requestSignature =
              zipHeaderPairs
                [ ("headers", headers request)
                , ("keyId", keyId)
                , ("algorithm", "rsa-sha256")
                , ("signature", signature)
                ]
        return $
          setRequestHeader
            "authorization"
            ["Signature " <> requestSignature]
            request

--------------------------------------------------------------------------------
transformRequest :: Credentials.Credentials -> Request -> IO Request
transformRequest credentials request =
  let keyId = Credentials.keyId credentials
  in addGenericHeaders request >>= (flip addAuthHeader keyId)

----------------------------------------------------------------------

compartment = "ocid1.compartment.oc1..aaaaaaaa3um2atybwhder4qttfhgon4j3hcxgmsvnyvx4flfjyewkkwfzwnq"

listInstances :: BS.ByteString -> Request
listInstances compartmentId =
  setRequestHost "iaas.us-phoenix-1.oraclecloud.com" $
  setRequestSecure True $ setRequestPort 443 $
  setRequestPath "/20160918/instances" $
  setRequestQueryString [("compartmentId", Just compartmentId)] $
  defaultRequest

binRequest compartmentId =
  setRequestHost "requestb.in" $
  setRequestPath "/10r0cri1" $
  setRequestQueryString [("compartmentId", Just compartmentId)] $ defaultRequest

testing = do
  credentials <- Credentials.defaultCredentialsProvider
  req <- transformRequest credentials (binRequest compartment)
  return req

dumpHeaders req =
  forM_ (requestHeaders req) $ do
    \(k, v) -> putStrLn $ (show k) ++ ":" ++ (show v)

testRequest = do
  credentials <- Credentials.defaultCredentialsProvider
  req <-
    transformRequest
      credentials
      (listInstances
         "ocid1.compartment.oc1..aaaaaaaa3um2atybwhder4qttfhgon4j3hcxgmsvnyvx4flfjyewkkwfzwnq")
  return req

main = testRequest >>= httpLbs
