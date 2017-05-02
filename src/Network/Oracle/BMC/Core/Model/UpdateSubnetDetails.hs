{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.UpdateSubnetDetails
  ( UpdateSubnetDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data UpdateSubnetDetails = UpdateSubnetDetails
  {
  } deriving (Show)

instance FromJSON UpdateSubnetDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
