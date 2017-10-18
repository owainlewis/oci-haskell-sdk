{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.UpdateRouteTableRequest where

data UpdateRouteTableRequest = UpdateRouteTableRequest
  {
  } deriving (Eq, Show)

instance ToRequest UpdateRouteTableRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []
