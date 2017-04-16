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
  , CredentialsProvider
  , configFileCredentialsProvider
  , defaultCredentialsProvider
  , getKeyId
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.IO as TIO

import Network.Oracle.BMC.Internal.Exception
       (throwLeftIO, BMCException(..))

import Control.Applicative ((<$>))
import Control.Exception
import Data.Ini (Ini, lookupValue, parseIni)
import Data.Semigroup ((<>))
import System.Environment (getEnv)

import System.Directory (getHomeDirectory)

import qualified Data.ByteString as BS

type CredentialError = String

type CredentialsProvider = IO Credentials

-- | Expands a shorthand path expression i.e ~/Workspace will be expanded
--   to an absolute path including the users home directory
--
expandPath :: FilePath -> IO FilePath
expandPath p = do
  home <- getHomeDirectory
  return $
    case p of
      ('~':t) -> home ++ t
      _ -> p

data BMCCredentials = BMCCredentials
  { bmcUser :: T.Text
  , bmcFingerprint :: T.Text
  , bmcKeyFile :: T.Text
  , bmcTenancy :: T.Text
  } deriving (Eq, Read, Show)

data Credentials = Credentials
  { user :: T.Text -- User OCID
  , fingerprint :: T.Text -- Fingerprint of private SSH key
  , key :: T.Text -- Raw private SSH key contents
  , tenancy :: T.Text -- Tenancy OCID
  } deriving (Eq, Read, Show)

-- | Extract a credentials object
--
parseBMCCredentials :: T.Text -> Ini -> Either CredentialError BMCCredentials
parseBMCCredentials key ini = do
  user <- lookupValue key "user" ini
  fingerprint <- lookupValue key "fingerprint" ini
  keyFile <- lookupValue key "key_file" ini
  tenancy <- lookupValue key "tenancy" ini
  return $ BMCCredentials user fingerprint keyFile tenancy

configFileBMCSCredentialsProvider :: FilePath
                                  -> T.Text
                                  -> IO (Either String BMCCredentials)
configFileBMCSCredentialsProvider path key = do
  contents <- TIO.readFile path
  return $ (parseIni contents) >>= (parseBMCCredentials key)

-- Load credentials including private SSH key raw
--
-- This provides everything needed to authenticate a user
--
configFileCredentialsProvider :: FilePath -> T.Text -> IO Credentials
configFileCredentialsProvider path key =
  let eitherCredentials = do
        expandedPath <- expandPath path
        creds <- configFileBMCSCredentialsProvider expandedPath key
        case creds of
          Left e -> return . Left $ InvalidCredentialsException e
          Right (BMCCredentials u f k t) -> do
            expandedKeyPath <- expandPath (T.unpack k)
            sshKeyRaw <- TIO.readFile expandedKeyPath
            return . Right $ Credentials u f sshKeyRaw t
  in throwLeftIO eitherCredentials

-- | Handler for the most common case where credentials are stored in the default location
--   with the default profile
--
defaultCredentialsProvider :: IO Credentials
defaultCredentialsProvider =
  configFileCredentialsProvider "~/.oraclebmc/config" "DEFAULT"

-- | Generate the keyId from a set of credentials. A BMCS key takes the form
--
-- >> <TENANCY OCID>/<USER OCID>/<KEY FINGERPRINT>
--
getKeyId :: Credentials -> BS.ByteString
getKeyId (Credentials u f _ t) = Encoding.encodeUtf8 $ t <> "/" <> u <> "/" <> f
