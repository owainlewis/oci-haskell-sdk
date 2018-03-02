module Network.HTTP.Signature where

import           Network.HTTP.Client (Request)
import           System.Process

signWithOpenSSLCommand privateKeyPath input =
    mconcat ["echo ", input, " | openssl dgst -sha256 -sign | openssl enc -e -base64 | tr -d '\n'", privateKeyPath]

test privateKeyPath input = readCreateProcessWithExitCode (shell cmd) []
    where cmd = signWithOpenSSLCommand privateKeyPath input

-- main = signWithPrivateKey "/Users/owainlewis/.oci/oci_api_key.pem" "Hello World"
