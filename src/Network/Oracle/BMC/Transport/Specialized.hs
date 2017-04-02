{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Transport.Specialized where

import qualified Network.HTTP.Client                  as Client
import qualified Network.Oracle.BMC.Transport.Request as Request
import qualified Network.URI as URI

import Data.Maybe(fromMaybe)

import qualified Data.ByteString                      as BS

import qualified Data.Monoid as Monoid

import           Data.Time                            (defaultTimeLocale,
                                                       formatTime, getZonedTime)

getNow :: IO String
getNow = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" <$> getZonedTime

genericHeaders :: [BS.ByteString]
genericHeaders = ["(request-target)", "host", "date"]

bodyHeaders :: [BS.ByteString]
bodyHeaders = ["x-content-sha256", "content-type", "content-length"]

getRequestPath :: Request.HttpRequest -> Maybe String
getRequestPath request = URI.uriPath <$> URI.parseURI (Request.url request)

--withRequestTarget request@(Request.HttpRequest m u h b q) =
--    fmap (\p -> show m ++ p ++ (fromMaybe "" q)) (getRequestPath request)
