{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Network.HTTP.Signer
  ( signWithPrivateKey
  ) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.CaseInsensitive  (original)
import           Data.Char             (toLower)
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import           Data.Time
import qualified Network.HTTP.Client   as H
import qualified Network.HTTP.Types    as H
import           System.Process

import           Network.HTTP.Simple

import           Control.Arrow


defaultGenericHeaders :: [BS.ByteString]
defaultGenericHeaders = ["date", "(request-target)", "host"]

defaultBodyHeaders :: [BS.ByteString]
defaultBodyHeaders = ["content-length", "content-type", "x-content-sha256"]

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
      $ setRequestQueryString [("compartmentId", Just "ocid1.tenancy.oc1..aaaaaaaatyn7scrtwtqedvgrxgr2xunzeo6uanvyhzxqblctwkrpisvke4kq")]
      $ defaultRequest

example = doRequest demoRequest

-- | Sign an input string using open SSL and the private key supplied
--
signWithPrivateKey :: T.Text -> T.Text -> IO (Either T.Text T.Text)
signWithPrivateKey privateKeyPath input =
    let cmd = mconcat ["echo ", input, " | openssl dgst -sha256 -sign ", privateKeyPath, " | openssl enc -e -base64 | tr -d '\n'"]
    in do
      (_, stdin, stderr) <- readCreateProcessWithExitCode (shell . T.unpack $ cmd) []
      if stderr == "" then return . Right . T.pack $ stdin
                      else return . Left . T.pack $ stderr

-- | Adapted from Network.HTTP.Simple
setOneRequestHeader :: H.HeaderName -> BS.ByteString -> H.Request -> H.Request
setOneRequestHeader name val req =
    req { H.requestHeaders = filter (\(x, _) -> x /= name) (H.requestHeaders req) ++ [(name, val)] }

type HeaderTransformer = H.Request -> IO H.Request

-- For GET and DELETE requests (when there's no content in the request body),
-- the signing string must include at least these headers
-- host, date (request-target)

addRequestTargetHeader :: HeaderTransformer
addRequestTargetHeader request =
  pure $ setOneRequestHeader "(request-target)" target request
  where
    rMethod = lowerCaseBS (H.method request)
    lowerCaseBS = C8.pack . map toLower . C8.unpack
    target = rMethod <> " " <> (H.path request) <> (H.queryString request)

addHostHeader :: HeaderTransformer
addHostHeader request = pure (setOneRequestHeader "host" (H.host request) request)

addDateHeader :: HeaderTransformer
addDateHeader request = do
  now <-
    C8.pack <$> formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" <$>
    getCurrentTime
  return $ setOneRequestHeader "date" now request

addDefaultHeaders :: Request -> IO Request
addDefaultHeaders request = addDateHeader request >>= addHostHeader >>= addRequestTargetHeader

computeSignature :: Request -> BS.ByteString
computeSignature request = BS.intercalate "\n" hdrs
  where
    hdrs = map (\(k, v) -> original k <> ": " <> v) (H.requestHeaders request)

-- TODO addSignedRequestSignature
base64EncodedRequestSignature :: Request -> IO BS.ByteString
base64EncodedRequestSignature request = do
  let signature = computeSignature request
  return signature

zipHeaderPairs :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
zipHeaderPairs pairs = BS.intercalate "," $ map f pairs
  where
    f = (\(k, v) -> k <> "=\"" <> v <> "\"")

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

signRequest :: Request -> IO Request
signRequest req = do
  req' <- addDefaultHeaders req >>= (flip addAuthHeader "foo/bar/baz")
  return req'
