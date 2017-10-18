{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.GetCpeRequest where

data GetCpeRequest = GetCpeRequest
  {
  } deriving (Eq, Show)

instance ToRequest GetCpeRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []
