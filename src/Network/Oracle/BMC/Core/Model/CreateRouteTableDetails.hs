{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.CreateRouteTableDetails
  ( CreateRouteTableDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data CreateRouteTableDetails = CreateRouteTableDetails
  {
  } deriving (Show)

instance FromJSON CreateRouteTableDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
