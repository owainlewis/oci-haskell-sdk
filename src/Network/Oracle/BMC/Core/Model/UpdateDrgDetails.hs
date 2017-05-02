{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.UpdateDrgDetails
  ( UpdateDrgDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data UpdateDrgDetails = UpdateDrgDetails
  {
  } deriving (Show)

instance FromJSON UpdateDrgDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
