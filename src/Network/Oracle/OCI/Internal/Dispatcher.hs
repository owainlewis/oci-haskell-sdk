-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Oracle.OCI.Internal.Dispatcher
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
--
-- Module responsible for running HTTP requests and transforming them by adding the appropriate
-- authentication information.
--
-----------------------------------------------------------------------------
module Network.Oracle.OCI.Internal.Dispatcher
  ( runRequest
  , runRequestRaw
  , OCIAPIResponse
  ) where

import           Data.Aeson                                 (FromJSON, decode)
import           Network.HTTP.Client                        (Response,
                                                             responseBody,
                                                             responseStatus)
import           Network.HTTP.Simple                        (httpLBS)
import           Network.HTTP.Types.Status                  (statusCode)
import           Network.Oracle.OCI.Credentials
import           Network.Oracle.OCI.Internal.Exception      (OCIException (..))
import           Network.Oracle.OCI.Internal.Model.APIError
import           Network.Oracle.OCI.Internal.Request

import qualified Data.ByteString.Lazy                       as LBS

import           Control.Exception                          (throwIO)

type OCIAPIResponse a = IO (Either APIError a)

----------------------------------------------------------------------
throwEitherMaybe :: IO (Either (Maybe a) (Maybe b)) -> IO (Either a b)
throwEitherMaybe ioEither = do
  result <- ioEither
  case result of
    Left me  -> throwMaybe Left me
    Right ma -> throwMaybe Right ma
  where
    throwMaybe f ma =
      case ma of
        Just x  -> return (f x)
        Nothing -> throwIO (JSONParseException)

runRequestMaybe
  :: (ToRequest a, FromJSON b)
  => IO Credentials -> a -> IO (Either (Maybe APIError) (Maybe b))
runRequestMaybe credentialsProvider request = do
  credentials <- credentialsProvider
  response <- transform credentials (toRequest request) >>= httpLBS
  let responseCode = statusCode $ responseStatus response
  let outcome =
        if responseCode >= 400
          then Left <$> decode $ responseBody response
          else Right <$> decode $ responseBody response
  return outcome

-- | Run a HTTP request and return either an API Error (HTTP code 400+)
--   or a valid Aeson response
runRequest
  :: (ToRequest a, FromJSON b)
  => IO Credentials -> a -> OCIAPIResponse b
runRequest credentialsProvider request =
  throwEitherMaybe (runRequestMaybe credentialsProvider request)

-- | Run a request and return the raw response
--
runRequestRaw
  :: ToRequest a
  => IO Credentials -> a -> IO (Response LBS.ByteString)
runRequestRaw credentialsProvider request = do
  credentials <- credentialsProvider
  response <- transform credentials (toRequest request) >>= httpLBS
  return response
