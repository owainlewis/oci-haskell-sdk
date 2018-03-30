module Network.Oracle.OCI.Common.Request
  ( Request(..)
  , Method(..)
  ) where

import           Data.ByteString as BS

data Method = GET | PUT | POST | DELETE deriving ( Eq, Ord, Show )

data Request = Request {
    method  :: Method
  , headers :: [(String, String)]
  , body    :: BS.ByteString
}
