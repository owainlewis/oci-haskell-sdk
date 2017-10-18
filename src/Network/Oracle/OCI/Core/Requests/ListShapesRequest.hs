{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.ListShapesRequest where

data ListShapesRequest = ListShapesRequest
  {
  } deriving (Eq, Show)

instance ToRequest ListShapesRequest where
  toRequest request = error "Not defined"
  extractQuery _ = []
