{-# LANGUAGE DeriveDataTypeable #-}

module Network.Oracle.BMC.Exception where

import Control.Exception
import Data.Typeable

--------------------------------------------------------
data BareMetalCloudException =
  SimpleException String
  deriving (Typeable)

instance Show BareMetalCloudException where
  show (SimpleException e) = e

instance Exception BareMetalCloudException

------------------------------------------------------
-- | Convert Either errors into impure IO errors
throwLeftIO
  :: Show e
  => IO (Either e b) -> IO b
throwLeftIO ioe = do
  result <- ioe
  case result of
    Left e -> throwIO (SimpleException . show $ e)
    Right a -> return a
