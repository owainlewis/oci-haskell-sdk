{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.UpdateDrgAttachmentDetails
  ( UpdateDrgAttachmentDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data UpdateDrgAttachmentDetails = UpdateDrgAttachmentDetails
  {
  } deriving (Show)

instance FromJSON UpdateDrgAttachmentDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
