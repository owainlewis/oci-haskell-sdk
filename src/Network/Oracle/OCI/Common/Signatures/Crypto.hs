module Network.Oracle.OCI.Common.Signatures.Crypto where

import           Crypto.Hash             (Digest, SHA256 (..), hash)
import           Data.ByteArray.Encoding (Base (Base64), convertToBase)

getDigest :: BS.ByteString -> Digest SHA256
getDigest bs = hash bs
