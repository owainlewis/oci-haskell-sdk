{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.CreateVcnDetails
  ( CreateVcnDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data CreateVcnDetails = CreateVcnDetails
  {
  } deriving (Show)

instance FromJSON CreateVcnDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
