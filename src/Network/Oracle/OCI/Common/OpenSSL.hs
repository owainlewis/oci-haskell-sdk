{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Network.Oracle.OCI.Common.OpenSSL
  ( signWithPrivateKey )
  where

import qualified Control.Exception     as E
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           System.Process        (readCreateProcessWithExitCode, shell)

data OCISDKException = SignException String
    deriving Show

instance E.Exception OCISDKException

-- | Sign an input string using open SSL and the private key supplied
--
signWithPrivateKey :: String -> String -> IO BS.ByteString
signWithPrivateKey privateKeyPath input =
    let cmd = concat ["echo \"", input, "\" | openssl dgst -sha256 -sign ", privateKeyPath, " | openssl enc -e -base64 | tr -d '\n'"]
    in do
      (_, stdin, stderr) <- readCreateProcessWithExitCode (shell cmd) []
      if stderr == "" then return (C8.pack stdin)
                      else E.throw (SignException stderr)
