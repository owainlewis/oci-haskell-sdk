{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Signature
    ( sign
    , signWithKey
    , signBase64
    , loadPrivateKey
    , parsePrivateKey
    ) where

import           Crypto.Types.PubKey.RSA (PrivateKey, PublicKey)
import           Crypto.PubKey.OpenSsh   (OpenSshPrivateKey (..), decodePrivate)

import qualified Codec.Crypto.RSA.Pure   as RSA
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy.Char8   as C
import qualified Data.ByteString.Base64  as B64

import Data.Bifunctor(bimap)

data SignatureException = InvalidRSAKeyException
                        | KeyReadException
                        deriving ( Eq, Read, Show )

defaultKeyPath :: FilePath
defaultKeyPath = "/home/owainlewis/.oraclebmc/bmcs_api_key.pem"

throwLeft :: Either String OpenSshPrivateKey -> PrivateKey
throwLeft (Right (OpenSshPrivateKeyRsa k)) = k
throwLeft (Right _) = error ("Invalid RSA key type")
throwLeft (Left e)  = error ("Error reading private key " ++ e)

loadPrivateKey :: FilePath -> IO PrivateKey
loadPrivateKey keyPath = (throwLeft . decodePrivate) <$> BS.readFile keyPath

parsePrivateKey = throwLeft . decodePrivate

sign :: FilePath -> BS.ByteString -> IO (Either RSA.RSAError BS.ByteString)
sign keyPath input = loadPrivateKey keyPath >>= (flip signWithKey input)

signWithKey :: Monad m => PrivateKey -> BS.ByteString -> m (Either RSA.RSAError BS.ByteString)
signWithKey key input = do
    let signature = RSA.sign key (C.fromStrict input)
    case signature of
        Left e -> return $ Left e
        Right sig -> return $ Right (C.toStrict sig)

signBase64 :: FilePath -> BS.ByteString -> IO (Either RSA.RSAError BS.ByteString)
signBase64 keyPath input = do
    signature <- sign keyPath input
    let base64EncodedSignature = bimap (id) (B64.encode) signature
    return base64EncodedSignature
