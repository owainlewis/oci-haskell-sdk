{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.UpdateSubnetRequest where

data UpdateSubnetRequest = UpdateSubnetRequest
  {
  } deriving (Eq, Show)

instance ToRequest UpdateSubnetRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []
