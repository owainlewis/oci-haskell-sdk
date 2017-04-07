{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Network.Oracle.BMC.Signature
--
-- Maintainer  : Owain Lewis <owain@owainlewis.com>
-- Stability   : experimental
-- Portability : GHC
--
--
module Network.Oracle.BMC.Signature
    ( signWithKey
    , sign
    , signBase64
    , signBase64Unsafe
    , loadPrivateKey
    , SignatureException(..)
    ) where

import           Crypto.PubKey.OpenSsh      (OpenSshPrivateKey (..),
                                             decodePrivate)
import           Crypto.Types.PubKey.RSA    (PrivateKey, PublicKey)

import qualified Codec.Crypto.RSA.Pure      as RSA
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Lazy.Char8 as C

import           Data.Bifunctor             (bimap)

data SignatureException = InvalidRSAKeyException
                        | KeyReadException
                        deriving ( Eq, Read, Show )

extractPrivateKey :: Either String OpenSshPrivateKey -> PrivateKey
extractPrivateKey (Right (OpenSshPrivateKeyRsa k)) = k
extractPrivateKey (Right _) = error ("Invalid RSA key type")
extractPrivateKey (Left e)  = error ("Error reading private key " ++ e)

loadPrivateKey :: FilePath -> IO PrivateKey
loadPrivateKey keyPath =
    (extractPrivateKey . decodePrivate) <$> BS.readFile keyPath

sign :: FilePath -> BS.ByteString -> IO (Either RSA.RSAError BS.ByteString)
sign keyPath input = (flip signWithKey input) <$> loadPrivateKey keyPath

signWithKey :: PrivateKey -> BS.ByteString -> Either RSA.RSAError BS.ByteString
signWithKey key input = bimap id (C.toStrict) (RSA.sign key (C.fromStrict input))

-- | Sign a byte string with a private key, the path to which is provided as input
--
signBase64 :: FilePath -> BS.ByteString -> IO (Either RSA.RSAError BS.ByteString)
signBase64 keyPath input = do
    signature <- sign keyPath input
    let base64EncodedSignature = bimap (id) (B64.encode) signature
    return base64EncodedSignature

-- | Lazy programming. Should sort this out
signBase64Unsafe :: FilePath -> BS.ByteString -> IO BS.ByteString
signBase64Unsafe keyPath input = do
    e <- signBase64 keyPath input
    case e of
      Left (RSA.RSAError e) -> error e
      Right signature -> return signature
