{-# LANGUAGE OverloadedStrings #-}
module Network.Oracle.BMC.Signature
    ( sign
    , loadPrivateKey
    ) where

import qualified Codec.Crypto.RSA           as RSA
import qualified Codec.Crypto.RSA.Pure      as Pure
import           Crypto.Types.PubKey.RSA    (PrivateKey, PublicKey)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as C

import           Data.Maybe                 (fromJust)
import           OpenSSL.EVP.PKey           (toKeyPair)
import           OpenSSL.PEM                (PemPasswordSupply (..),
                                             readPrivateKey)
import           OpenSSL.RSA

defaultKeyPath :: FilePath
defaultKeyPath = ".oraclebmc/bmcs_api_key.pem"

loadKeyPair :: FilePath -> IO RSAKeyPair
loadKeyPair location = do
      key <- flip readPrivateKey PwNone =<< readFile location
      let maybeRsaKey = (toKeyPair key) :: Maybe RSAKeyPair
      return $ fromJust maybeRsaKey

toPrivateKey :: RSAKeyPair -> PrivateKey
toPrivateKey rsaKeyPair =
  let s = rsaSize rsaKeyPair
      n = rsaN rsaKeyPair
      e = rsaE rsaKeyPair
      d = rsaD rsaKeyPair
      p = rsaP rsaKeyPair
      q = rsaQ rsaKeyPair
      dp = d `mod` (p-1)
      dq = d `mod` (q-1)
      qinv = 0
  in
  RSA.PrivateKey (RSA.PublicKey s n e) d p q dp dq qinv

loadPrivateKey :: FilePath -> IO PrivateKey
loadPrivateKey location = toPrivateKey <$> loadKeyPair location

sign :: PrivateKey -> BS.ByteString -> BS.ByteString
sign privateKey input = C.toStrict $ RSA.sign privateKey (C.fromStrict input)

signPure :: BS.ByteString -> IO (Either Pure.RSAError BS.ByteString)
signPure input = do
    key <- loadPrivateKey ("/home/owainlewis/" ++ defaultKeyPath)
    return $ C.toStrict <$> Pure.sign key (C.fromStrict input)

input = "Hello, World!"

out = "\ESC\189\164<\250\144]\DC4a\167Xs\142 Z\DC2\240^\174\200x#\204\203\SUB\211\SOH0,\225C\221\132\&0\US\149a\152\249mzl=\235\152r\196v\GSC!\SOH\189\194\253\144\t\255\140\233\SI'\DC1`\229\134\r\165\141%\v\nI\238\SYN\141FW/\\\244\172\RS\222/\250M,\207\250Q2\DC2\GS\SI\204\DEL\181\238\242\a\EM\166\ESC\156\147lI\DLE\216+\FS\130\208\209\161:\200\226\215cw\211\241\133\ENQ\213\199{2\242\248*\144\253\SYN`\220|5\211\177<\149v\208\205\172~\223\151\SOH*\242\232\251&\248\&5\162\&3U\128M\214\200\186@\SYNj\150\246\135\219\186\ETB\224\232\SIm\vv\v\218\SYN\150c\EM\223\&0K\167\141RTH\130\153\NAK}\b\198\227R6Y4\175^;\160\241\179b~L\248_a\180\225\158\184\210\218\167l\228\144}eJ\140\135\233\150b\GSN%\162\214n\150sw\207)\v9\190\DC4G\172<5"

signandverify :: lbs.bytestring -> lbs.bytestring -> io (either pure.rsaerror bool)
signandverify input output = do
    (rsa.privatekey pub _ _ _ _ _ _) <- loadprivatekey ("/home/owainlewis/" ++ defaultkeypath)
    return $ pure.verify (pub) input output


