{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Transport.Request
    ( HttpRequest(..)
    , HttpResponse
    , HttpMethod(..)
    , URL
    , Header
    , runHttpsRequest
    , putHeader
    , putHeaderIfAbsent
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

import Data.Set as Set hiding ( delete )

-- | Types
-------------------------------------------------------------------------------

type URL     = String

type Header  = (BS.ByteString, BS.ByteString)

data HttpMethod = GET
                | POST
                | PUT
                | DELETE
                | PATCH
                | HEAD
                deriving ( Eq, Read, Show )

data HttpRequest = HttpRequest { httpMethod :: HttpMethod
                               , url :: String
                               , headers :: [Header]
                               , body :: Maybe BS.ByteString
                               , query :: Maybe [(BS.ByteString, BS.ByteString)]
                               } deriving ( Eq, Read, Show )

type HttpResponse = Network.HTTP.Client.Response LBS.ByteString

-- | Request contsruction
-------------------------------------------------------------------------------

-- | Construct a HTTP GET request
--
--   You can also set the query string using a list of (k,v) tuples
--
-- > λ> (get "https://github.com" []) { query = Just [("foo", "bar")] }
--
-- > HttpRequest {
--     httpMethod = GET
--   , url = "https://github.com"
--   , headers = []
--   , body = Nothing
--   , query = Nothing
--   }
--
get :: URL -> [Header] -> HttpRequest
get url headers = HttpRequest GET url headers Nothing Nothing

post :: URL -> [Header] -> Maybe BS.ByteString -> HttpRequest
post url headers body = HttpRequest POST url headers body Nothing

put :: URL -> [Header] -> Maybe BS.ByteString -> HttpRequest
put url headers body = HttpRequest PUT url headers body Nothing

delete :: URL -> [Header] -> HttpRequest
delete url headers = HttpRequest DELETE url headers Nothing Nothing

-------------------------------------------------------------------------------

-- | TODO This isn't a great datastructure to be using. Given there aren't many
--   http headers the performance is less of an issue but users having to import
--   containers as a library was the main concern
--
putHeader :: HttpRequest -> Header -> HttpRequest
putHeader request header =
    request { headers = header : (headers request) }

putHeaderIfAbsent :: HttpRequest -> Header -> HttpRequest
putHeaderIfAbsent request header =
    if headerExists (headers request) header then putHeader request header
                                             else request
      where headerExists [] h = False
            headerExists (x:xs) h =
                if x == h then True else headerExists xs h

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
                              , requestHeaders = Prelude.map (\(k, v) -> (CI.mk k, v)) (headers request)
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
