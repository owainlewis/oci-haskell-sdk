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
import           Control.Exception

import qualified Network.Oracle.BMC.Path as Path

type CredentialError = String

-- | The default location for Oracle BMC credentials
--
defaultLocation :: IO FilePath
defaultLocation = Path.expand "~/.oraclebmc/config"

data BMCCredentials = BMCCredentials {
    bmcUser        :: T.Text
  , bmcFingerprint :: T.Text
  , bmcKeyFile     :: T.Text
  , bmcTenancy     :: T.Text
} deriving ( Eq, Read, Show )

data Credentials = Credentials {
    user        :: T.Text  -- User OCID
  , fingerprint :: T.Text  -- Fingerprint of private SSH key
  , key         :: T.Text  -- Raw private SSH key contents
  , tenancy     :: T.Text  -- Tenancy OCID
} deriving ( Eq, Read, Show )

-- | Extract a credentials object
--
parseBMCCredentials :: T.Text -> Ini -> Either CredentialError BMCCredentials
parseBMCCredentials key ini = do
    user <- lookupValue key "user" ini
    fingerprint <- lookupValue key "fingerprint" ini
    keyFile <- lookupValue key "key_file" ini
    tenancy <- lookupValue key "tenancy" ini
    let credentials = BMCCredentials user fingerprint keyFile tenancy
    return credentials

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
loadCredentials :: IO (Either CredentialError Credentials)
loadCredentials = do
    creds <- loadDefaultBMCCredentials
    case creds of
        Left e -> return (Left e)
        Right (BMCCredentials u f k t) -> do
            -- TODO (OL) Catch the IO exception and pass to Left
            expandedKeyPath <- Path.expand (T.unpack k)
            sshKeyRaw <- TIO.readFile expandedKeyPath
            return . Right $ Credentials u f sshKeyRaw t

-- | Generate the keyId from a set of credentials. A BMCS key takes the form
--
-- >> <TENANCY OCID>/<USER OCID>/<KEY FINGERPRINT>
--
keyId :: Credentials -> T.Text
keyId (Credentials u f _ t) = t <> "/" <> u <> "/" <> f
