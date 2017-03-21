{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Oracle.BMC.Credentials
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
--
-- This module defines readers and types for Oracle Bare Metal Cloud credentials
--
-----------------------------------------------------------------------------
module Network.Oracle.BMC.Credentials
    ( Credentials(..)
    , BMCCredentials(..)
    , fromFile
    , loadDefaultBMCCredentials
    , parseBMCCredentials
    , loadCredentials
    ) where

import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO

import           Control.Applicative ((<$>))
import           Data.Ini            (Ini, lookupValue, parseIni)
import           Data.Semigroup      ((<>))
import           System.Environment  (getEnv)

import Control.Exception

type CredentialError = String

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
data BMCCredentials = BMCCredentials {
    bmcUser        :: T.Text
  , bmcFingerprint :: T.Text
  , bmcKeyFile     :: T.Text
  , bmcTenancy     :: T.Text
} deriving ( Eq, Show )

data Credentials = Credentials {
    user        :: T.Text  -- User OCID
  , fingerprint :: T.Text  -- Fingerprint of private SSH key
  , key         :: T.Text  -- Raw private SSH key contents
  , tenancy     :: T.Text  -- Tenancy OCID
} deriving ( Eq, Show )

-- | Extract a credentials object
--
parseBMCCredentials :: T.Text -> Ini -> Either CredentialError BMCCredentials
parseBMCCredentials key ini = do
    user <- lookupValue key "user" ini
    fingerprint <- lookupValue key "fingerprint" ini
    keyFile <- lookupValue key "key_file" ini
    tenancy <- lookupValue key "tenancy" ini
    return $ BMCCredentials user fingerprint keyFile tenancy

fromFile :: FilePath -> IO (Either CredentialError BMCCredentials)
fromFile path = do
    contents <- TIO.readFile path
    return $ (parseIni contents) >>= (parseBMCCredentials "DEFAULT")

loadDefaultBMCCredentials :: IO (Either CredentialError BMCCredentials)
loadDefaultBMCCredentials = fromFile =<< defaultLocation

-- Load credentials including private SSH key raw
--
-- This provides everything needed to authenticate a user
--
-- This will fail on unexpanded paths for SSH key (~/.oraclebmc) and if the key
-- does not exist
loadCredentials :: IO (Either CredentialError Credentials)
loadCredentials = do
    creds <- loadDefaultBMCCredentials
    case creds of
        Left e -> return (Left e)
        Right (BMCCredentials u f k t) -> do
            -- TODO (OL) Catch the IO exception and pass to Left
            sshKeyRaw <- catch (TIO.readFile (T.unpack k)) (\e -> return "")
            return . Right $ Credentials u f sshKeyRaw t
