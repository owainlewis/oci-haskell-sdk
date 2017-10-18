{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.IngressSecurityRule
  ( IngressSecurityRule(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data IngressSecurityRule = IngressSecurityRule
  {
  } deriving (Show)

instance FromJSON IngressSecurityRule where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
