{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
--- |
--- Module      :  Network.Oracle.OCI.Common.Credentials
--- License     :  BSD-style (see the file LICENSE)
---
--- Maintainer  :  Owain Lewis <owain.lewis@oracle.com>
---
--- This module loads credentials.

--- Typically this means reading the ~/.oci/config ini file
---
------------------------------------------------------------------------------
module Network.Oracle.OCI.Common.Credentials
  ( Credentials(..)
  , KeyProvider
  , KeyID
  , getKeyID
  , newCredentials
  , parseCredentials
  , readCredentialsFromFile
  ) where

import qualified Data.ByteString   as BS
import           Data.Monoid       ((<>))
import qualified Data.Text         as T

import qualified Control.Exception as E
import           Data.Ini          (Ini, lookupValue, parseIni)
import qualified Data.Text.IO      as TIO

import           Data.Typeable     (Typeable)

type KeyID = T.Text

class KeyProvider a where
  getKeyID :: a -> KeyID

data Credentials = Credentials
  { user        :: T.Text
  , tenancy     :: T.Text
  , region      :: T.Text
  , keyFile     :: T.Text
  , fingerprint :: T.Text
  , passphrase  :: Maybe T.Text
  } deriving (Eq, Read, Show)

newCredentials
  :: T.Text
  -> T.Text
  -> T.Text
  -> T.Text
  -> T.Text
  -> Maybe T.Text
  -> Credentials
newCredentials u t r k f p = Credentials u t r k f p

instance KeyProvider Credentials where
  getKeyID (Credentials user tenancy _ _ fingerprint _) =
      tenancy <> "/" <> user <> "/" <> fingerprint

lookupMaybe :: T.Text -> T.Text -> Ini -> Either String (Maybe T.Text)
lookupMaybe k v ini =
  case lookupValue k v ini of
    Left e  -> Right Nothing
    Right v -> Right (Just v)

parseIniCredentials :: T.Text -> Ini -> Either String Credentials
parseIniCredentials key ini = do
  user <- lookupValue key "user" ini
  tenancy <- lookupValue key "tenancy" ini
  region <- lookupValue key "region" ini
  keyFile <- lookupValue key "key_file" ini
  fingerprint <- lookupValue key "fingerprint" ini
  passphrase <- lookupMaybe key "passphrase" ini
  return $ Credentials user tenancy region keyFile fingerprint passphrase

parseCredentials :: T.Text
  -> T.Text
  -> Either String Credentials
parseCredentials contents section =
    parseIni contents >>= (parseIniCredentials section)

data CredentialsException = CredentialsException String
    deriving (Show, Typeable)

instance E.Exception CredentialsException

readCredentialsFromFile :: FilePath
  -> String
  -> IO Credentials
readCredentialsFromFile filePath section = do
    result <- flip parseCredentials (T.pack section) <$> TIO.readFile filePath
    case result of
      Left e            -> E.throwIO (CredentialsException . show $ e)
      Right credentials -> return credentials
