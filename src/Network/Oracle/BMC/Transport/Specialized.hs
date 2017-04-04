{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Transport.Specialized where

import qualified Network.HTTP.Client                  as Client
import           Network.Oracle.BMC.Transport.Request (HttpRequest)
import qualified Network.Oracle.BMC.Transport.Request as Request
import qualified Network.URI                          as URI

import           Data.Maybe                           (fromMaybe)

import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Char8                as C8

import qualified Data.Monoid                          as Monoid

import           Network.Oracle.BMC.Credentials       (Credentials (..))

import           Data.Time                            (defaultTimeLocale,
                                                       formatTime, getZonedTime)

import qualified Network.Oracle.BMC.Core.Instance     as Instance

getNow :: IO String
getNow = formatTime defaultTimeLocale tf <$> getZonedTime
    where tf = "%a, %d %b %Y %H:%M:%S %Z"

-- Required on all get | delete requests
--
genericHeaders :: [BS.ByteString]
genericHeaders = ["(request-target)", "host", "date"]

-- Required on all post | put | patch requests
--
bodyHeaders :: [BS.ByteString]
bodyHeaders = ["x-content-sha256", "content-type", "content-length"]

getRequestPath :: HttpRequest -> String
getRequestPath request = fromMaybe Monoid.mempty maybePath
    where maybePath = (URI.uriPath <$> URI.parseURI (Request.url request))

-- Adds the date header to a request
--
withDateHeader :: HttpRequest -> IO HttpRequest
withDateHeader request = do
    now <- C8.pack <$> getNow
    return $ Request.putHeader request ("date", now)

withRequestTargetHeader :: HttpRequest -> HttpRequest
withRequestTargetHeader request =
    let m = show (Request.httpMethod request)
        p = getRequestPath request
        -- TODO fixme
        q = ""
        requestTarget = concat [m, " ", p, " ", q]
    in Request.putHeader request ("(request-target)", C8.pack requestTarget)

withHostHeader :: HttpRequest -> HttpRequest
withHostHeader request = fromMaybe request (withMaybeHost request)
    where withMaybeHost request = let u = Request.url request in
            do
              uri <- URI.parseURI (Request.url request)
              authority <- URI.uriAuthority uri
              let host = URI.uriRegName authority
              return $ Request.putHeader request ("host", C8.pack host)

withAuthHeader request = request

withJsonHeader request = Request.putHeader request ("content-type", "application/json")

addMissingHeaders :: HttpRequest -> IO HttpRequest
addMissingHeaders request = do
    withDateHeader request >>=
      pure . withRequestTargetHeader >>=
      pure . withHostHeader

sampleRequest = Instance.list compartment
   where compartment = "ocid1.compartment.oc1..aaaaaaaa3um2atybwhder4qttfhgon4j3hcxgmsvnyvx4flfjyewkkwfzwnq"


-- 1. Create the signing string, which is based on parts of the request.

createSigningString request = request

-- 2. Create the signature from the signing string, using your private key and the RSA-SHA256 algorithm.

-- 3. Add the resulting signature and other required information to the Authorization header in the request.

