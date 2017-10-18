{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.UpdateIPSecConnectionDetails
  ( UpdateIPSecConnectionDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data UpdateIPSecConnectionDetails = UpdateIPSecConnectionDetails
  {
  } deriving (Show)

instance FromJSON UpdateIPSecConnectionDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
