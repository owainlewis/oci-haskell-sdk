{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
-- | A fairly naive wrapper around openssl. TODO this would be better using the Haskell crypto libs
module Network.Oracle.OCI.Common.Signatures.OpenSSL
  ( signWithPrivateKey
  , getSHA256Digest
  )
  where

import qualified Control.Exception     as E
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Typeable         (Typeable)
import           System.Process        (readCreateProcessWithExitCode, shell)

data OCISDKException = RequestSignException String
    deriving (Show, Typeable)

instance E.Exception OCISDKException

quoted :: String -> String
quoted s = "\"" ++ s ++ "\""

doCommand :: String -> IO BS.ByteString
doCommand cmd = do
    (_, stdin, stderr) <- readCreateProcessWithExitCode (shell cmd) []
    if stderr == "" then return (C8.pack stdin)
                    else E.throwIO (RequestSignException stderr)

-- | So dirty. Fix up at some point using Haskell crypto
getSHA256Digest :: String -> IO BS.ByteString
getSHA256Digest input = doCommand cmd
  where cmd = concat [ "printf '%b' "
                     , quoted input
                     , " | openssl dgst -binary -sha256 | openssl enc -e -base64 | tr -d '\n'"
                     ]

-- | Sign an input string using open SSL and the private key supplied.
--   This is non ideal but neither are the Haskell crypto libs I tried.
signWithPrivateKey :: FilePath -> String -> IO BS.ByteString
signWithPrivateKey privateKeyPath input = doCommand cmd
  where cmd = concat [ "printf '%b' "
                     , quoted input
                     , " | openssl dgst -sha256 -sign "
                     , privateKeyPath
                     , " | openssl enc -e -base64 | tr -d '\n'"
                     ]
