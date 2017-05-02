{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.CreateInternetGatewayRequest where

data CreateInternetGatewayRequest = CreateInternetGatewayRequest
  {
  } deriving (Eq, Show)

instance ToRequest CreateInternetGatewayRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []
