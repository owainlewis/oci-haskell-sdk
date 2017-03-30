module Network.Oracle.Internal.Util where

import Data.Char
import Data.Word
import Data.Array.Unboxed
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Char8 as C

ctype_lower = listArray (0,255) (map (BI.c2w . toLower) ['\0'..'\255']) :: UArray Word8 Word8

lowercase = B.map (\x -> ctype_lower!x)
