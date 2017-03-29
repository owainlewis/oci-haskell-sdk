module Network.Oracle.BMC.Path
    ( expand )
    where

import           System.Directory (getHomeDirectory)

-- | Expands a shorthand path expression i.e ~/Workspace will be expanded
--   to an absolute path including the users home directory
--
expand :: FilePath -> IO FilePath
expand p = do
    home <- getHomeDirectory
    return $ case p of
      ('~' : t) -> home ++ t
      _         -> p
