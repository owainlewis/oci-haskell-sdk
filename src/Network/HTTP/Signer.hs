{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Network.HTTP.Signer
  ( signWithPrivateKey
  ) where

import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as C8
import           Data.Char             (toLower)
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import           Data.Time
import qualified Network.HTTP.Client   as H
import qualified Network.HTTP.Types    as H
import           System.Process

import           Network.HTTP.Simple

import           Control.Arrow

doRequest :: H.Request -> IO Int
doRequest request = do
  response <- httpLBS request
  return $ getResponseStatusCode response

demoRequest :: H.Request
demoRequest =
        setRequestHost "https://identity.us-ashburn-1.oraclecloud.com"
      $ setRequestPath "/20160918/compartments"
      $ setRequestQueryString [("compartmentId", Just "ocid1.tenancy.oc1..aaaaaaaatyn7scrtwtqedvgrxgr2xunzeo6uanvyhzxqblctwkrpisvke4kq")]
      $ setRequestSecure True
      $ setRequestPort 443
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
setOneRequestHeader :: H.HeaderName -> S.ByteString -> H.Request -> H.Request
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
