{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.UpdateImageDetails
  ( UpdateImageDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data UpdateImageDetails = UpdateImageDetails
  {
  } deriving (Show)

instance FromJSON UpdateImageDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
