{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Transport.Request
    ( addGenericHeaders
    ) where

import qualified Data.ByteString.Char8 as C8
import           Data.Char             (toLower)
import           Data.Semigroup        ((<>))
import           Data.Time             (defaultTimeLocale, formatTime,
                                        getZonedTime)
import           Network.HTTP.Client   (Request (..))
import           Network.HTTP.Simple

addDateHeader :: Request -> IO Request
addDateHeader request = do
    now <- C8.pack <$> formatTime defaultTimeLocale tf <$> getZonedTime
    return $ setRequestHeader "date" [now] request
    where tf = "%a, %d %b %Y %H:%M:%S %Z"

addRequestTargetHeader :: Request -> Request
addRequestTargetHeader request =
    setRequestHeader "(request-target)" [target] request
    where
      rMethod = lowerCaseBS (method request)
      lowerCaseBS = C8.pack . map toLower . C8.unpack
      target = rMethod <> " " <> (path request) <> (queryString request)

addHostHeader :: Request -> Request
addHostHeader request = setRequestHeader "host" [(host request)] request

addGenericHeaders :: Request -> IO Request
addGenericHeaders request =
    addDateHeader request >>=
    pure . addRequestTargetHeader >>=
    pure . addHostHeader

-- | Form the authentication signature header
--
addAuthHeader request =
    let rqHeaders = requestHeaders request in
      rqHeaders
-- addAuthHeader <$> addGenericHeaders example

----------------------------------------------------------------------

listInstances compartmentId =
    setRequestHost "https://iaas.us-phoenix-1.oraclecloud.com" $
    setRequestPath "/20160918/instances" $
    setRequestQueryString [("compartmentId", Just compartmentId)]
    defaultRequest

example = listInstances "ocid.compatment"

----------------------------------------------------------------------
