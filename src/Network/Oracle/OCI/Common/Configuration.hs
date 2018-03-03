{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.OCI.Common.Configuration
  ( Credentials(..)
  , KeyProvider
  , KeyID
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
  keyID :: a -> KeyID

data Credentials = Credentials
  { user        :: T.Text
  , tenancy     :: T.Text
  , region      :: T.Text
  , keyFile     :: T.Text
  , fingerprint :: T.Text
  , passphrase  :: Maybe T.Text
  } deriving (Eq, Read, Show)

instance KeyProvider Credentials where
  keyID (Credentials user tenancy _ _ fingerprint _) = tenancy <> "/" <> user <> "/" <> fingerprint

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
parseCredentials contents section = parseIni contents >>= (parseIniCredentials section)

data CredentialsException = CredentialsException String
    deriving (Show, Typeable)

instance E.Exception CredentialsException

readCredentialsFromFile :: FilePath
  -> T.Text
  -> IO Credentials
readCredentialsFromFile filePath section = do
    result <- flip parseCredentials section <$> TIO.readFile filePath
    case result of
      Left e            -> E.throwIO (CredentialsException . show $ e)
      Right credentials -> return credentials
