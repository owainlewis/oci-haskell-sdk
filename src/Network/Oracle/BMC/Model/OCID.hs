module Network.Oracle.BMC.Model.OCID 
  ( ocidPattern
  ) where

ocidPattern = "^([0-9a-zA-Z-_]+[.:])([0-9a-zA-Z-_]*[.:]){3,}([0-9a-zA-Z-_]+)$"

isOCID :: String -> Bool
isOCID = error "TODO"
