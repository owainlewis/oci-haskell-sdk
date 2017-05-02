{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.CaptureConsoleHistoryDetails
  ( CaptureConsoleHistoryDetails(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data CaptureConsoleHistoryDetails = CaptureConsoleHistoryDetails
  {
  } deriving (Show)

instance FromJSON CaptureConsoleHistoryDetails where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
