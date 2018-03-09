module Network.Oracle.OCI.Common.Region
  ( Region(..)
  ) where

data Region =
      Phoenix
    | Ashburn
    | Frankfurt
    deriving (Eq, Ord)

instance Show Region where
  show (Phoenix) = "us-phoenix-1"
  show (Ashburn) = "us-ashburn-1"
  show (Frankfurt) = "eu-frankfurt-1"
