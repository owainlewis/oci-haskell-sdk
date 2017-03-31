{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Transport.Request
    ( HttpRequest(..)
    , HttpResponse
    , HttpMethod(..)
    , URL
    , Headers
    , runHttpsRequest
    , get
    , post
    , put
    , delete
    ) where

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as C8
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.CaseInsensitive      as CI

-- | Types
-------------------------------------------------------------------------------

type URL     = String

type Headers = [(BS.ByteString, BS.ByteString)]

data HttpMethod = GET
                | POST
                | PUT
                | DELETE
                | PATCH
                | HEAD
                deriving ( Eq, Read, Show )

data HttpRequest = HttpRequest { httpMethod :: HttpMethod
                               , url :: String
                               , headers :: [(BS.ByteString, BS.ByteString)]
                               , body :: Maybe BS.ByteString
                               , query :: Maybe [(BS.ByteString, BS.ByteString)]
                               } deriving ( Eq, Read, Show )


type HttpResponse = Network.HTTP.Client.Response LBS.ByteString
-- | Construct a HTTP GET request
--
--   You can also set the query string
--
-- > λ> (get "https://github.com" []) { query = Just [] }
--
-- > HttpRequest {
--     httpMethod = GET
--   , url = "https://github.com"
--   , headers = []
--   , body = Nothing
--   , query = Just []
--   }
--
get :: URL -> Headers -> HttpRequest
get url headers = HttpRequest GET url headers Nothing Nothing

post :: URL -> Headers -> Maybe BS.ByteString -> HttpRequest
post url headers body = HttpRequest POST url headers body Nothing

put :: URL -> Headers -> Maybe BS.ByteString -> HttpRequest
put url headers body = HttpRequest PUT url headers body Nothing

delete :: URL -> Headers -> HttpRequest
delete url headers = HttpRequest DELETE url headers Nothing Nothing

-- | Request / Response
-------------------------------------------------------------------------------

transformRequest :: HttpRequest -> IO Network.HTTP.Client.Request
transformRequest request =
      let secondJust = (\(k, v) -> (k, Just v)) in
        do
            manager <- newManager tlsManagerSettings
            initialRequest <- parseRequest (url request)
            let initReq = initialRequest
                              { method = C8.pack (show . httpMethod $ request)
                              , requestHeaders = map (\(k, v) -> (CI.mk k, v)) (headers request)
                              }
                req = maybe initReq (\qs -> setQueryString (secondJust <$> qs) initReq) (query request)
            return $ maybe req (\lbs -> req { requestBody = RequestBodyBS lbs }) (body request)

-- | TODO pass options (i.e proxy stuff) into here

runHttpsRequest :: HttpRequest -> IO HttpResponse
runHttpsRequest req = do
    manager <- newManager tlsManagerSettings
    internalRequest <- transformRequest req
    response <- httpLbs internalRequest manager
    return response
