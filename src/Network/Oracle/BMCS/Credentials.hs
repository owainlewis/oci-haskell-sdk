{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Oracle.BMCS.Credentials
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
--
-- This module defined readers and types for Oracle BMCS credentials
-----------------------------------------------------------------------------
module Network.Oracle.BMCS.Credentials
    ( Credentials(..)
    ) where

import Data.Text as T
import Data.Text.IO as TIO
import System.Environment(getEnv)
import Data.Semigroup((<>))
import Data.Ini

defaultLocation :: IO FilePath
defaultLocation = (\homeDir -> homeDir <> "/.oraclebmc/config") <$> getEnv "HOME"

-- Example credentials
--   [DEFAULT]
--   user=ocid1.user.oc1..aaaaaaaat5nvwcna5j6aqzjcmdy5eqbb6qt2jvpkanghtgdaqedqw3rynja
--   fingerprint=20:3b:97:13:55:1c:5b:0d:d3:37:d8:50:4e:c5:3a:39
--   key_file=~/.oraclebmc/bmcs_api_key.pem
--   tenancy=ocid1.tenancy.oc1..aaaaaaaaba3pv6wkcr4jqae5f15p2bcmdyt2j6rx32uzr6h25vqstifsfdsq
--
data Credentials = Credentials {
    user :: T.Text
  , fingerprint :: T.Text
  , keyFile :: T.Text
  , tenancy :: T.Text
} deriving ( Eq, Show )

parseCredentials :: Ini -> Either String Credentials
parseCredentials ini = do
    user <- lookupValue "DEFAULT" "user" ini
    fingerprint <- lookupValue "DEFAULT" "fingerprint" ini
    keyFile <- lookupValue "DEFAULT" "key_file" ini
    tenancy <- lookupValue "DEFAULT" "tenancy" ini
    return $ Credentials user fingerprint keyFile tenancy

fromFile :: FilePath -> IO (Either String Credentials)
fromFile path = do
    contents <- TIO.readFile path
    return $ (parseIni contents) >>= parseCredentials
