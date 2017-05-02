{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.UpdateDhcpOptionsRequest where

data UpdateDhcpOptionsRequest = UpdateDhcpOptionsRequest
  {
  } deriving (Eq, Show)

instance ToRequest UpdateDhcpOptionsRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []
