{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch where

import           Network.HTTP.Simple

main :: IO ()
main = do
    request' <- parseRequest "https://httpbin.org"
    let request
            = setRequestMethod "GET"
            $ setRequestPath "/get"
            $ setRequestSecure True
            $ setRequestPort 443
            $ request'
    response <- httpLBS request
    putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
