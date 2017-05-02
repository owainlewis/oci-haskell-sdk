{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.UpdateCpeDetails
  ( UpdateCpeDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data UpdateCpeDetails = UpdateCpeDetails
  {
  } deriving (Show)

instance FromJSON UpdateCpeDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
