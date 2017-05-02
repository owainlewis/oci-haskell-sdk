{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.CreateSecurityListDetails
  ( CreateSecurityListDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data CreateSecurityListDetails = CreateSecurityListDetails
  {
  } deriving (Show)

instance FromJSON CreateSecurityListDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
