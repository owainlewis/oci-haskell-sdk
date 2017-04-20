{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Network.Oracle.BMC.Internal.Signature
--
-- Maintainer  : Owain Lewis <owain@owainlewis.com>
-- Stability   : experimental
-- Portability : GHC
--
-- This module is used to sign strings using a private key. This functionality is
-- required in order to implement the HTTP authentication specification
-----------------------------------------------------------------------------
module Network.Oracle.BMC.Internal.Signature
  ( signBase64
  ) where

import Crypto.PubKey.OpenSsh (OpenSshPrivateKey(..), decodePrivate)
import Crypto.Types.PubKey.RSA (PrivateKey, PublicKey)

import qualified Codec.Crypto.RSA.Pure as RSA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8 as C

import Control.Exception (throwIO)
import Network.Oracle.BMC.Internal.Exception
       (throwLeftIO, BMCException(..))

import Data.Bifunctor (second, bimap)

loadPrivateKey :: FilePath -> IO PrivateKey
loadPrivateKey keyPath = do
  decoded <- decodePrivate <$> BS.readFile keyPath
  case decoded of
    (Right (OpenSshPrivateKeyRsa k)) -> return k
    (Right _) -> throwIO . GenericException $ "Invalid RSA key type"
    (Left e) -> throwIO . GenericException $ "Error reading private key " ++ e

sign :: FilePath -> BS.ByteString -> IO (Either RSA.RSAError BS.ByteString)
sign keyPath input = (flip signWithKey input) <$> loadPrivateKey keyPath

signWithKey :: PrivateKey -> BS.ByteString -> Either RSA.RSAError BS.ByteString
signWithKey key input = second (C.toStrict) (RSA.sign key (C.fromStrict input))

-- | Sign a byte string with a private key, the path to which is provided as input
signBase64 :: FilePath -> BS.ByteString -> IO BS.ByteString
signBase64 keyPath input =
  let signature =
        bimap (RSASignatureException . show) (B64.encode) <$> sign keyPath input
  in throwLeftIO signature
