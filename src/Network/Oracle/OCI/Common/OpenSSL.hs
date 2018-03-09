{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Network.Oracle.OCI.Common.OpenSSL
  ( signWithPrivateKey )
  where

import qualified Control.Exception     as E
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Typeable         (Typeable)
import           System.Process        (readCreateProcessWithExitCode, shell)

data OCISDKException = RequestSignException String
    deriving (Show, Typeable)

instance E.Exception OCISDKException

-- | Sign an input string using open SSL and the private key supplied
--
signWithPrivateKey :: String -> String -> IO BS.ByteString
signWithPrivateKey privateKeyPath input =
    let quoted s = "\"" ++ s ++ "\""
        cmd = concat [ "printf '%b' "
                     , quoted input
                     , " | openssl dgst -sha256 -sign "
                     , privateKeyPath
                     , " | openssl enc -e -base64 | tr -d '\n'"
                     ]
    in do
      (_, stdin, stderr) <- readCreateProcessWithExitCode (shell cmd) []
      if stderr == "" then return (C8.pack stdin)
                      else E.throwIO (RequestSignException stderr)
