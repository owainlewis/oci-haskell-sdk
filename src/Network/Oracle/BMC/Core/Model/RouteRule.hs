{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.RouteRule
  ( RouteRule(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data RouteRule = RouteRule
  {
  } deriving (Show)

instance FromJSON RouteRule where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
