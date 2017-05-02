{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.RouteTable
  ( RouteTable(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data RouteTable = RouteTable
  {
  } deriving (Show)

instance FromJSON RouteTable where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
