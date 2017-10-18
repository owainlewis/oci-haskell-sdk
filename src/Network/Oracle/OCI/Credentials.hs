{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Oracle.OCI.Credentials
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
--
-- This module defines readers and types for Oracle Bare Metal Cloud credentials
--
-----------------------------------------------------------------------------
module Network.Oracle.OCI.Credentials
  ( Credentials(..)
  , CredentialsProvider
  , configFileCredentialsProvider
  , defaultCredentialsProvider
  , getKeyId
  , getKeyPath
  ) where

import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as Encoding
import qualified Data.Text.IO                          as TIO

import           Network.Oracle.OCI.Internal.Exception (OCIException (..),
                                                        throwLeftIO)

import           Control.Applicative                   ((<$>))
import           Control.Exception
import           Data.Ini                              (Ini, lookupValue,
                                                        parseIni)
import           Data.Semigroup                        ((<>))
import           System.Environment                    (getEnv)

import           System.Directory                      (getHomeDirectory)

import qualified Data.ByteString                       as BS

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
      _       -> p

data Credentials = Credentials
  { user        :: T.Text -- User OCID
  , fingerprint :: T.Text -- Fingerprint of private SSH key
  , key         :: T.Text -- Raw private SSH key contents
  , tenancy     :: T.Text -- Tenancy OCID
  } deriving (Eq, Read, Show)

-- | Extract a credentials object
--
parseOCICredentials :: T.Text -> Ini -> Either CredentialError Credentials
parseOCICredentials key ini = do
  user <- lookupValue key "user" ini
  fingerprint <- lookupValue key "fingerprint" ini
  keyFile <- lookupValue key "key_file" ini
  tenancy <- lookupValue key "tenancy" ini
  return $ Credentials user fingerprint keyFile tenancy

configFileOCISCredentialsProvider :: FilePath
                                  -> T.Text
                                  -> IO (Either String Credentials)
configFileOCISCredentialsProvider path key = do
  contents <- TIO.readFile path
  return $ (parseIni contents) >>= (parseOCICredentials key)

-- Load credentials from file. This also expands the user home path
-- Will throw an exception is the config path is invalid and a file
-- does not exist
unsafeConfigFileCredentialsProvider :: FilePath -> T.Text -> IO Credentials
unsafeConfigFileCredentialsProvider path key =
  let eitherCredentials = do
        expandedPath <- expandPath path
        creds <- configFileOCISCredentialsProvider expandedPath key
        case creds of
          Left e -> return . Left $ InvalidCredentialsException e
          Right (Credentials u f k t) -> do
            expandedKeyPath <- expandPath (T.unpack k)
            return . Right $ Credentials u f (T.pack expandedKeyPath) t
  in throwLeftIO eitherCredentials

configFileCredentialsProvider :: FilePath -> T.Text -> IO Credentials
configFileCredentialsProvider = unsafeConfigFileCredentialsProvider

-- | Handler for the most common case where credentials are stored in the default location
--   with the default profile
--
defaultCredentialsProvider :: IO Credentials
defaultCredentialsProvider =
  configFileCredentialsProvider "~/.oci/config" "DEFAULT"

-- | Generate the keyId from a set of credentials. A OCI key takes the form
--
-- >> <TENANCY OCID>/<USER OCID>/<KEY FINGERPRINT>
--
getKeyId :: Credentials -> BS.ByteString
getKeyId (Credentials u f _ t) = Encoding.encodeUtf8 $ t <> "/" <> u <> "/" <> f

getKeyPath :: Credentials -> String
getKeyPath = T.unpack . key
