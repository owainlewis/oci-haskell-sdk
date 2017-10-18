{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.DrgAttachment
  ( DrgAttachment(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data DrgAttachment = DrgAttachment
  {
  } deriving (Show)

instance FromJSON DrgAttachment where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
