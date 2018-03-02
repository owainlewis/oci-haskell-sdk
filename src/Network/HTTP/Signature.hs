{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Network.HTTP.Signature
  ( signWithPrivateKey
  ) where


import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text             as T
import           Data.Time
import qualified Network.HTTP.Client   as H
import qualified Network.HTTP.Types    as H
import           System.Process

-- | Sign an input string using open SSL and the private key supplied
--
signWithPrivateKey :: T.Text -> T.Text -> IO (Either T.Text T.Text)
signWithPrivateKey privateKeyPath input =
    let cmd = mconcat ["echo ", input, " | openssl dgst -sha256 -sign ", privateKeyPath, " | openssl enc -e -base64 | tr -d '\n'"]
    in do
      (_, stdin, stderr) <- readCreateProcessWithExitCode (shell . T.unpack $ cmd) []
      if stderr == "" then return . Right . T.pack $ stdin
                      else return . Left . T.pack $ stderr

-- | Adapted from Network.HTTP.Simple
setRequestHeader :: H.HeaderName -> S.ByteString -> H.Request -> H.Request
setRequestHeader name val req =
    req { H.requestHeaders = filter (\(x, _) -> x /= name) (H.requestHeaders req) ++ [(name, val)] }

type HeaderTransformer = H.Request -> IO H.Request

addDateHeader :: HeaderTransformer
addDateHeader request = do
  now <-
    C8.pack <$> formatTime defaultTimeLocale "%a, %_d %b %Y %H:%M:%S GMT" <$>
    getCurrentTime
  return $ setRequestHeader "date" now request
