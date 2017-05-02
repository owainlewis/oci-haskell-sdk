{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.InstanceActionRequest where

data InstanceActionRequest = InstanceActionRequest
  {
  } deriving (Eq, Show)

instance ToRequest InstanceActionRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []
