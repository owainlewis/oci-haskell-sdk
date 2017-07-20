import os

SDK_LOCATION = "/home/owainlewis/Workspace/bmcs-java-sdk"

REQUEST_TEMPLATE="""{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Requests.%s where

import qualified Data.ByteString as BS
import Data.Semigroup ((<>))
import Network.HTTP.Simple
import Network.Oracle.BMC.Core.Requests.Base (mkBaseRequest)
import Network.Oracle.BMC.Internal.Query
import Network.Oracle.BMC.Internal.Request

data %s = %s {

} deriving ( Eq, Show )

instance ToRequest %s where
    toRequest request = error "Not defined"
    extractQuery _ = []

"""


MODEL_TEMPLATE = """
{-# LANGUAGE OverloadedStrings #-}

module Network.Oracle.BMC.Core.Model.%s
  ( %s(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson

data %s = %s { } deriving (Show)

instance FromJSON %s where
  parseJSON (Object v) = error "TODO"
  parseJSON _ = mzero
"""


def java_files_in(location):
    return [f for f in os.listdir(location) if f.endswith(".java")]


def list_request_files():
    location = SDK_LOCATION + \
                '/bmc-core/src/main/java/com/oracle/bmc/core/requests'
    return java_files_in(location)


def list_model_files():
    location = SDK_LOCATION + \
               '/bmc-core/src/main/java/com/oracle/bmc/core/model'
    return java_files_in(location)


def equivalent_haskell_file_exists(file_name):
    equivalent = file_name.replace("java", "hs")
    return os.path.isfile(equivalent)


def generate_file_in_directory(directory, file_name, template):
    tn = file_name.split('.')[0]
    file_name = file_name.replace("java", "hs")
    print('Generating %s' % file_name)
    with open(directory + "/" + file_name, 'w') as f:
        f.write(template % (tn, tn, tn, tn, tn))


def generate_code(files, directory, template):
    for file_name in files:
        eqe = equivalent_haskell_file_exists(directory + "/" + file_name)
        if not eqe:
            generate_file_in_directory(directory, file_name, template)


def main():
    generate_code(list_model_files(), 
                "src/Network/Oracle/BMC/Core/Model",
                 MODEL_TEMPLATE)


if __name__ == '__main__':
    main()
