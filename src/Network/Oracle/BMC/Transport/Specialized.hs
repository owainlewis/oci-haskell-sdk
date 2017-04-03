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

import           Data.Time                            (defaultTimeLocale,
                                                       formatTime, getZonedTime)

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
withDate :: HttpRequest -> IO HttpRequest
withDate request = do
    now <- C8.pack <$> getNow
    return $ Request.putHeaderIfAbsent request ("date", now)

withRequestTarget :: HttpRequest -> HttpRequest
withRequestTarget request =
    let m = show (Request.httpMethod request)
        p = getRequestPath request
        q = ""
        requestTarget = concat [m, " ", p, " ", q]
    in Request.putHeaderIfAbsent request ("(request-target)", C8.pack requestTarget)

withMaybeHost request =
  let u = Request.url request in
  do
    uri <- URI.parseURI (Request.url request)
    authority <- URI.uriAuthority uri
    let host = URI.uriRegName authority
    return $ Request.putHeaderIfAbsent request ("host", C8.pack host)

withHost :: HttpRequest -> HttpRequest
withHost request = fromMaybe request (withMaybeHost request)

addMissingHeaders :: HttpRequest -> IO HttpRequest
addMissingHeaders request = do
    withDate request >>= pure . withRequestTarget
