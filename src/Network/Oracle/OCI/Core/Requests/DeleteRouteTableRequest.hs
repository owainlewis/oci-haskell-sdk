{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.DeleteRouteTableRequest where

data DeleteRouteTableRequest = DeleteRouteTableRequest
  {
  } deriving (Eq, Show)

instance ToRequest DeleteRouteTableRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []
