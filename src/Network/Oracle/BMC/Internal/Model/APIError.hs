{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Oracle.BMC.Internal.Model.Error
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
--
-- Common domain model for API errors returned by BMCS
--
-----------------------------------------------------------------------------
module Network.Oracle.BMC.Internal.Model.APIError where

import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString as BS
import GHC.Generics

data APIError = APIError
  { code :: String
  , message :: String
  } deriving (Eq, Generic, Show)
  
