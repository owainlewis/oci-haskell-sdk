{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Transport.Specialized where

import qualified Network.HTTP.Client as Client
import qualified Network.Oracle.BMC.Transport.Request as Request


import           Data.Time                            (defaultTimeLocale,
                                                       formatTime, getZonedTime)

getNow :: IO String
getNow = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" <$> getZonedTime

genericHeaders = ["(request-target)"
                 , "host"
                 , "date"
                 ]

bodyHeaders = [ "x-content-sha256"
              , "content-type"
              , "content-length"
              ]

withRequestTarget :: Client.Request -> Client.Request
withRequestTarget request =
      let path = Client.path request
          query = Client.queryString request
          method = Client.method request
      in path

      
