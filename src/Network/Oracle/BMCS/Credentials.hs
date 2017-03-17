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
    , fromFile
    , parseCredentials
    , parseCredentialsFor
    ) where

import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO

import           Data.Ini
import           Data.Semigroup     ((<>))
import           System.Environment (getEnv)

defaultLocation :: IO FilePath
defaultLocation = (\homeDir -> homeDir <> "/.oraclebmc/config") <$> getEnv "HOME"

-- Example credentials
--
-- >  [DEFAULT]
-- >  user=ocid1.user.oc1..aaaaaaaat5nv...
-- >  fingerprint=20:3b:97:13:55:1c:5b:0d:d3:37:d8:50:4e:c5:3a:39
-- >  key_file=~/.oraclebmc/bmcs_api_key.pem
-- >  tenancy=ocid1.tenancy.oc1..aaaaaaaaba3pv6wkcr4jqae5f1...
--
data Credentials = Credentials {
    user        :: T.Text
  , fingerprint :: T.Text
  , keyFile     :: T.Text
  , tenancy     :: T.Text
} deriving ( Eq, Show )

-- | Extract a credentials object
--
parseCredentialsFor :: String -> Ini -> Either String Credentials
parseCredentialsFor key ini = do
    user <- lookupValue key "user" ini
    fingerprint <- lookupValue key "fingerprint" ini
    keyFile <- lookupValue key "key_file" ini
    tenancy <- lookupValue key "tenancy" ini
    return $ Credentials user fingerprint keyFile tenancy

parseCredentials :: Ini -> Either String Credentials
parseCredentials = parseCredentialsfor "DEFAULT"

fromFile :: FilePath -> IO (Either String Credentials)
fromFile path = do
    contents <- TIO.readFile path
    return $ (parseIni contents) >>= parseCredentials
