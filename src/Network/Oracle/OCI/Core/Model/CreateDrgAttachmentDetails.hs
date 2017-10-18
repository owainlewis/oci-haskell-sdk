{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.CreateDrgAttachmentDetails
  ( CreateDrgAttachmentDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data CreateDrgAttachmentDetails = CreateDrgAttachmentDetails
  {
  } deriving (Show)

instance FromJSON CreateDrgAttachmentDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
