{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.APIError where

import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString as BS

data APIError = APIError
  { code :: String
  , message :: String
  } deriving (Eq, Show)

instance FromJSON APIError where
  parseJSON (Object v) = APIError <$> v .: "code" <*> v .: "message"
  parseJSON _ = mzero
