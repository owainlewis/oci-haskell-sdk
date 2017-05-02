{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.UpdateSecurityListDetails
  ( UpdateSecurityListDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data UpdateSecurityListDetails = UpdateSecurityListDetails
  {
  } deriving (Show)

instance FromJSON UpdateSecurityListDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
