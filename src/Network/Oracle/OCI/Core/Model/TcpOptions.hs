{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.TcpOptions
  ( TcpOptions(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data TcpOptions = TcpOptions
  {
  } deriving (Show)

instance FromJSON TcpOptions where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
