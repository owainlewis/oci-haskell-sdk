module Network.Oracle.OCI.Model.OCID
  ( ocidPattern
  ) where

ocidPattern :: String
ocidPattern = "^([0-9a-zA-Z-_]+[.:])([0-9a-zA-Z-_]*[.:]){3,}([0-9a-zA-Z-_]+)$"
