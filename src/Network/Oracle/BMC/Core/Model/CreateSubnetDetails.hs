{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.CreateSubnetDetails
  ( CreateSubnetDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data CreateSubnetDetails = CreateSubnetDetails
  {
  } deriving (Show)

instance FromJSON CreateSubnetDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
