{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Signature where

import qualified Codec.Crypto.RSA        as RSA
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.ByteString.Lazy.Char8 as C
import           Crypto.Types.PubKey.RSA (PrivateKey, PublicKey)

import OpenSSL.PEM
import OpenSSL.EVP.PKey
import OpenSSL.RSA
import Data.Maybe

defaultPath = "/home/owainlewis/.oraclebmc/bmcs_api_key.pem"

loadKey :: IO RSAKeyPair
loadKey = do
      keyString <- readFile defaultPath
      key <- readPrivateKey keyString PwNone
      let maybeRsaKey = (toKeyPair key) :: Maybe RSAKeyPair
      return $ fromJust maybeRsaKey

publicKey kp =
  let size = 2048
      n = rsaN (kp)
      e = rsaE (kp)
      d = rsaD (kp)
      p = rsaP (kp)
      q = rsaQ (kp)
      dp = d `mod` (p-1)
      dq = d `mod` (q-1)
      qinv = (q^(-1)) `mod` p
      publicKeyRSA = RSA.PublicKey size n e
  in
  RSA.PrivateKey publicKeyRSA d p q dp dq qinv

signInput input = (\k -> RSA.sign (publicKey k) input) <$> loadKey

-- -- Sign and input with some private key
-- --
--signWithKey :: FilePath -> LBS.ByteString -> IO (LBS.ByteString)
--signWithKey keyPath input = flip RSA.sign input <$> loadKey

-- -- Used to sign HTTP request to the Oracle BMCS API

-- import qualified Codec.Crypto.RSA        as RSA

-- import           Codec.Crypto.RSA        (sign)
-- import           Crypto.PubKey.OpenSsh   (OpenSshPrivateKey (..), decodePrivate)
-- import           Crypto.Types.PubKey.RSA (PrivateKey)

-- import qualified Data.ByteString         as BS
-- import qualified Data.ByteString.Lazy    as LBS

-- import qualified Data.PEM as Pem

-- import qualified OpenSSL.PEM as P
-- import qualified OpenSSL.RSA as R

-- throwLeftIO :: (Monad m, Show a) => m (Either a b) -> m b
-- throwLeftIO either = do
--   x <- either
--   case x of
--     Left e  -> error (show e)
--     Right x -> return x

-- loadPrivateKey :: FilePath -> IO (Either String PrivateKey)
-- loadPrivateKey keyPath = do
--     contents <- BS.readFile keyPath
--     case (decodePrivate contents) of
--       Right (OpenSshPrivateKeyRsa k) -> return $ Right k
--       Right x                        -> return $ Left $ "E"
--       Left e                         -> return $ Left "Fail"

-- loadSSHPrivateKey :: FilePath -> IO PrivateKey
-- loadSSHPrivateKey = throwLeftIO . loadPrivateKey

-- -- Sign and input with some private key
-- --
-- signWithKey :: FilePath -> LBS.ByteString -> IO (LBS.ByteString)
-- signWithKey keyPath input = flip RSA.sign input <$> loadSSHPrivateKey keyPath

-- defaultPath = "/home/owainlewis/.oraclebmc/bmcs_api_key.pem"




