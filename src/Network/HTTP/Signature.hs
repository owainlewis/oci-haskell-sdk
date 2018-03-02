{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Signature
  ( signWithPrivateKey
  ) where

import qualified Data.Text           as T
import           Data.Time
import           Network.HTTP.Client (Request (..))
import           System.Process

signWithOpenSSLCommand :: T.Text -> T.Text -> T.Text
signWithOpenSSLCommand privateKeyPath input =
    mconcat ["echo ", input, " | openssl dgst -sha256 -sign ", privateKeyPath, " | openssl enc -e -base64 | tr -d '\n'"]

-- | Sign an input string using open SSL and the private key supplied
--
signWithPrivateKey :: T.Text -> T.Text -> IO (Either T.Text T.Text)
signWithPrivateKey privateKeyPath input = do
    (_, stdin, stderr) <- readCreateProcessWithExitCode (shell . T.unpack $ signWithOpenSSLCommand privateKeyPath input) []
    if stderr == "" then return . Right . T.pack $ stdin
                    else return . Left . T.pack $ stderr
