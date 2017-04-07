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
    , parseBMCCredentials
    , configFileCredentialsProvider
    , keyId
    ) where

import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Text.Encoding as Encoding

import           Control.Applicative ((<$>))
import           Control.Exception
import           Data.Ini            (Ini, lookupValue, parseIni)
import           Data.Semigroup      ((<>))
import           System.Environment  (getEnv)

import           System.Directory    (getHomeDirectory)

import qualified Data.ByteString as BS

type CredentialError = String

-- | Expands a shorthand path expression i.e ~/Workspace will be expanded
--   to an absolute path including the users home directory
--
expandPath :: FilePath -> IO FilePath
expandPath p = do
    home <- getHomeDirectory
    return $ case p of
      ('~' : t) -> home ++ t
      _         -> p

-- | The default location for Oracle BMC credentials
--
defaultLocation :: IO FilePath
defaultLocation = expandPath "~/.oraclebmc/config"

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
    return $ BMCCredentials user fingerprint keyFile tenancy

configFileBMCSCredentialsProvider :: FilePath ->
                                     T.Text ->
                                     IO (Either String BMCCredentials)
configFileBMCSCredentialsProvider path key = do
    contents <- TIO.readFile path
    return $ (parseIni contents) >>= (parseBMCCredentials key)

-- Load credentials including private SSH key raw
--
-- This provides everything needed to authenticate a user
--
configFileCredentialsProvider :: FilePath ->
                                 T.Text ->
                                 IO (Either String Credentials)
configFileCredentialsProvider path key = do
    creds <- configFileBMCSCredentialsProvider path key
    case creds of
        Left e -> return (Left e)
        Right (BMCCredentials u f k t) -> do
            -- TODO (OL) Catch the IO exception and pass to Left
            expandedKeyPath <- expandPath (T.unpack k)
            sshKeyRaw <- TIO.readFile expandedKeyPath
            return . Right $ Credentials u f sshKeyRaw t

-- | Generate the keyId from a set of credentials. A BMCS key takes the form
--
-- >> <TENANCY OCID>/<USER OCID>/<KEY FINGERPRINT>
--
keyId :: Credentials -> BS.ByteString
keyId (Credentials u f _ t) = Encoding.encodeUtf8 $ t <> "/" <> u <> "/" <> f
