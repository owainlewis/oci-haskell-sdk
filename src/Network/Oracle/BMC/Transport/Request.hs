{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Transport.Request
    ( HttpRequest(..)
    , HttpMethod(..)
    , runHttpsRequest
    ) where

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as C8
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.CaseInsensitive      as CI

-- | Types
-----------------------------------------------------------------------------------------

data HttpMethod = GET
                | POST
                | PUT
                | DELETE
                | PATCH
                deriving ( Eq, Read, Show )

data HttpRequest = HttpRequest { httpMethod :: HttpMethod
                               , url :: String
                               , headers :: [(BS.ByteString, BS.ByteString)]
                               , body :: Maybe BS.ByteString
                               } deriving ( Eq, Read, Show )

-- | Request / Response
-----------------------------------------------------------------------------------------

transformRequest :: HttpRequest -> IO Network.HTTP.Client.Request
transformRequest request = do
      manager <- newManager tlsManagerSettings
      initialRequest <- parseRequest (url request)
      let req = initialRequest
                    { method = C8.pack (show . httpMethod $ request)
                    , requestHeaders = map (\(k, v) -> (CI.mk k, v)) (headers request)
                    }
      return $ maybe req (\lbs -> req { requestBody = RequestBodyBS lbs }) (body request)

-- | TODO pass options into here
runHttpsRequest :: HttpRequest -> IO (Network.HTTP.Client.Response LBS.ByteString)
runHttpsRequest req = do
    manager <- newManager tlsManagerSettings
    internalRequest <- transformRequest req
    response <- httpLbs internalRequest manager
    return response

-- | Domain specific
-----------------------------------------------------------------------------------------

requiredHeaders = ["date", "(request-target)", "host"]

example = runHttpsRequest (HttpRequest GET url [] Nothing)
    where url = "https://iaas.us-phoenix-1.oraclecloud.com/20160918/instances?availabilityDomain=Pjwf%3A%20PHX-AD-1&compartmentId=ocid1.compartment.oc1..aaaaaaaam3we6vgnherjq5q2idnccdflvjsnog7mlr6rtdb25gilchfeyjxa&displayName=TeamXInstances&volumeId=ocid1.volume.oc1.phx.abyhqljrgvttnlx73nmrwfaux7kcvzfs3s66izvxf2h4lgvyndsdsnoiwr5q"
