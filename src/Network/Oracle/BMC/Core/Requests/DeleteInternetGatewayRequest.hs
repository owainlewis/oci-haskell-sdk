{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.DeleteInternetGatewayRequest where

data DeleteInternetGatewayRequest = DeleteInternetGatewayRequest
  {
  } deriving (Eq, Show)

instance ToRequest DeleteInternetGatewayRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []
