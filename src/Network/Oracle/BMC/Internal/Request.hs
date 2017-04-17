{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Oracle.BMC.Transport.Request
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
--
-- Add appropriate authentication information to a HTTP request based on
-- https://tools.ietf.org/html/draft-cavage-http-signatures-05
--
-----------------------------------------------------------------------------
module Network.Oracle.BMC.Internal.Request
  ( Path(..)
  , Query(..)
  , ToRequest(..)
  , unPath
  , unQuery
  , transform
  , flattenQuery
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
import qualified Network.Oracle.BMC.Internal.Signature as Signature

data Path a =
  Path a
  deriving (Eq, Show)

data Query a =
  Query a
  deriving (Eq, Show)

flattenQuery :: [(t, Maybe (Query a))] -> [(t, Maybe a)]
flattenQuery ls = [(k, Just . unQuery $ x) | (k, Just x) <- ls]

class ToRequest a where
  toRequest :: a -> Request
  extractQuery :: a -> [(BS.ByteString, Maybe BS.ByteString)]

-- | Extract the underlying value from a query
--
unQuery :: Query t -> t
unQuery (Query x) = x

-- | Extract the underlying value from a path
--
unPath :: Path t -> t
unPath (Path x) = x

--------------------------------------------------------------------------------
-- Add HTTP headers
--
-- These methods add the required HTTP headers to a request.
--
--------------------------------------------------------------------------------
type HeaderTransformer = Request -> IO Request

-- Add a HTTP date header. It appears this has to be in GMT
--
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
  addDateHeader request >>= addRequestTargetHeader >>= addHostHeader >>=
  addContentTypeDefault

--------------------------------------------------------------------------------
computeSignature :: Request -> BS.ByteString
computeSignature request = BS.intercalate "\n" hdrs
  where
    hdrs = map (\(k, v) -> original k <> ": " <> v) (requestHeaders request)

base64EncodedRequestSignature :: Request -> IO BS.ByteString
base64EncodedRequestSignature request = do
  let signature = computeSignature request
  Signature.signBase64 "/home/owainlewis/.oraclebmc/bmcs_api_key.pem" signature

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
  let headers = (BS.intercalate " ") . map (original . fst) . requestHeaders
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
-- | Take a normal HTTP request and add the appropritate authentication signature
--   information to it based on credentials provided
--
transform :: Credentials.Credentials -> Request -> IO Request
transform credentials request =
  let keyId = Credentials.getKeyId credentials
  in addGenericHeaders request >>= (flip addAuthHeader keyId)
