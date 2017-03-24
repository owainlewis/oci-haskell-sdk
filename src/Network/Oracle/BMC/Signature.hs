module Network.Oracle.BMC.Signature where

-- Used to sign HTTP request to the Oracle BMCS API

import qualified Codec.Crypto.RSA as RSA

import Codec.Crypto.RSA (sign)
import Crypto.PubKey.OpenSsh (decodePrivate, OpenSshPrivateKey(..))
import Crypto.Types.PubKey.RSA (PrivateKey)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

throwLeftIO :: (Monad m, Show a) => m (Either a b) -> m b
throwLeftIO either = do
  x <- either
  case x of
    Left e -> error (show e)
    Right x -> return x

loadPrivateKey :: FilePath -> IO (Either String PrivateKey)
loadPrivateKey keyPath = do
    contents <- BS.readFile keyPath
    case (decodePrivate contents) of
      Right (OpenSshPrivateKeyRsa k) -> return $ Right k
      Right _ -> return $ Left "Invalid key type"
      Left e -> return $ Left e

loadSSHPrivateKey :: FilePath -> IO PrivateKey
loadSSHPrivateKey = throwLeftIO . loadPrivateKey

-- Sign and input with some private key
signWithKey :: FilePath -> LBS.ByteString -> IO (LBS.ByteString)
signWithKey keyPath input = loadSSHPrivateKey keyPath >>= (\k -> return $ RSA.sign k input)

-- | Bytestrings probably easier here
--
signWithKey' :: FilePath -> BS.ByteString -> IO BS.ByteString
signWithKey' keyPath input = LBS.toStrict <$> signWithKey keyPath (LBS.fromStrict input)
