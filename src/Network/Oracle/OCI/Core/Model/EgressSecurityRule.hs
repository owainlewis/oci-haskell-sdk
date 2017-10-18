{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.EgressSecurityRule
  ( EgressSecurityRule(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data EgressSecurityRule = EgressSecurityRule
  {
  } deriving (Show)

instance FromJSON EgressSecurityRule where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
