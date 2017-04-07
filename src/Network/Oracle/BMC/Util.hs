module Network.Oracle.BMC.Util where

import Control.Exception(throwIO, Exception, toException)

data OracleBMCException = CredentialsException
    deriving (Typeable)

instance Show OracleBMCException where
    show CredentialsException = "Invalid credentials"

instance Exception OracleBMCException

-- | Convert Either errors into impure IO errors
throwLeftIO :: IO (Either e b) -> IO b
throwLeftIO ioe = do
    result <- ioe
    case result of
        Left e -> throwIO (Exception e)
        Right a -> return a
