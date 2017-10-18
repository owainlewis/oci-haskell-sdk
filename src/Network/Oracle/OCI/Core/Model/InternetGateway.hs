{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.InternetGateway
  ( InternetGateway(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data InternetGateway = InternetGateway
  {
  } deriving (Show)

instance FromJSON InternetGateway where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
