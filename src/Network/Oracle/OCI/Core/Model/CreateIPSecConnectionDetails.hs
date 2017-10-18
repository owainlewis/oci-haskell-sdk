{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.CreateIPSecConnectionDetails
  ( CreateIPSecConnectionDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data CreateIPSecConnectionDetails = CreateIPSecConnectionDetails
  {
  } deriving (Show)

instance FromJSON CreateIPSecConnectionDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
