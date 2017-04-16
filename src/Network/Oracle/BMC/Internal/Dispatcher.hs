module Network.Oracle.BMC.Internal.Dispatcher
  ( runRequest
  , BMCAPIResponse
  ) where

import Data.Aeson (FromJSON, decode)
import Network.HTTP.Client (Response, responseStatus, responseBody)
import Network.HTTP.Simple (httpLBS)
import Network.HTTP.Types.Status (statusCode)
import Network.Oracle.BMC.Credentials
import Network.Oracle.BMC.Internal.Exception (BMCException(..))
import Network.Oracle.BMC.Internal.Model.APIError
import Network.Oracle.BMC.Internal.Request

import Control.Exception (throwIO)

type BMCAPIResponse a = IO (Either APIError a)

----------------------------------------------------------------------
throwEitherMaybe :: IO (Either (Maybe a) (Maybe b)) -> IO (Either a b)
throwEitherMaybe ioEither = do
  result <- ioEither
  case result of
    Left me -> throwMaybe Left me
    Right ma -> throwMaybe Right ma
  where
    throwMaybe f ma =
      case ma of
        Just x -> return (f x)
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

-- | Run a HTTP request and return either an API Error (HTTP code 400+) or a valid response
--
runRequest
  :: (ToRequest a, FromJSON b)
  => IO Credentials -> a -> IO (Either APIError b)
runRequest credentialsProvider request =
  throwEitherMaybe (runRequestMaybe credentialsProvider request)
