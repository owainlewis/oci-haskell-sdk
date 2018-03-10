{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Oracle.OCI.Internal.Model.Error
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
--
-- Common domain model for API errors returned by OCIS
--
-----------------------------------------------------------------------------
module Network.Oracle.OCI.Common.Api
  ( APIError(..)
  ) where

import           Control.Monad   (mzero)
import           Data.Aeson
import qualified Data.ByteString as BS
import           GHC.Generics    (Generic)

data APIError = APIError { code :: String, message :: String }
    deriving (Eq, Generic, Show)

instance FromJSON APIError
instance ToJSON APIError
