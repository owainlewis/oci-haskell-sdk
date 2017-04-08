{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Transport.Request
  ( addGenericHeaders
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.CaseInsensitive (original)
import Data.Char (toLower)
import Data.Semigroup ((<>))
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Network.HTTP.Client (Request(..))
import Network.HTTP.Simple
import qualified Network.Oracle.BMC.Credentials as Credentials
import qualified Network.Oracle.BMC.Signature as Signature

--------------------------------------------------------------------------------
-- Add HTTP headers
--
-- These methods add the required HTTP headers to a request.
--
--------------------------------------------------------------------------------
type HeaderTransformer = Request -> IO Request

addDateHeader :: HeaderTransformer
addDateHeader request = do
  now <- C8.pack <$> formatTime defaultTimeLocale tf <$> getZonedTime
  return $ setRequestHeader "date" [now] request
  where
    tf = "%a, %d %b %Y %H:%M:%S %Z"

addRequestTargetHeader :: HeaderTransformer
addRequestTargetHeader request =
  pure $ setRequestHeader "(request-target)" [target] request
  where
    rMethod = lowerCaseBS (method request)
    lowerCaseBS = C8.pack . map toLower . C8.unpack
    target = rMethod <> " " <> (path request) <> (queryString request)

addHostHeader :: HeaderTransformer
addHostHeader request = pure $ setRequestHeader "host" [(host request)] request

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
base64EncodedRequestSignature request =
  Signature.signBase64Unsafe
    "/home/owainlewis/.oraclebmc/bmcs_api_key.pem"
    (computeSignature request)

zipHeaderPairs :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
zipHeaderPairs pairs = BS.intercalate " " $ map f pairs
  where
    f = (\(k, v) -> k <> "=\"" <> v <> "\"")

--------------------------------------------------------------------------------
-- | Adds the Authorization Signature HTTP header to a request which is
--   formed by signing the HTTP headers
--
addAuthHeader :: Request -> BS.ByteString -> IO Request
addAuthHeader request keyId =
  let headers = (BS.intercalate " ") . map (original . fst) . requestHeaders
  in do signature <- base64EncodedRequestSignature request
        let requestSignature =
              zipHeaderPairs
                [ ("version", "1")
                , ("headers", headers request)
                , ("keyId", keyId)
                , ("algorithm", "rsa-sha256")
                , ("signature", signature)
                ]
        return $
          setRequestHeader
            "Authorization"
            ["Signature " <> requestSignature]
            request

--------------------------------------------------------------------------------
transformRequest :: Credentials.Credentials -> Request -> IO Request
transformRequest credentials request =
  let keyId = Credentials.keyId credentials
  in addGenericHeaders request >>= (flip addAuthHeader keyId)

-- ----------------------------------------------------------------------
mkBaseRequest :: Request
mkBaseRequest =
  setRequestHost "iaas.us-phoenix-1.oraclecloud.com" $
  setRequestSecure True $ setRequestPort 443 $ defaultRequest

----------------------------------------------------------------------
listInstances compartmentId =
  setRequestPath "/20160918/instances" $
  setRequestQueryString [("compartmentId", Just compartmentId)] mkBaseRequest

requestExample = listInstances "ocid.123"

test = do
  credentials <- Credentials.defaultCredentialsProvider
  return $ transformRequest credentials requestExample
