import os

SDK_LOCATION = "/home/owainlewis/Workspace/bmcs-java-sdk"

TEMPLATE="""{-# LANGUAGE OverloadedStrings #-}

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

def get_java_files():
    d = SDK_LOCATION + '/bmc-core/src/main/java/com/oracle/bmc/core/requests'
    return [f for f in os.listdir(d) if f.endswith(".java")]


def generate_file(file_name):
    tn = file_name.split('.')[0]
    print('Generating %s' % file_name)
    with open(file_name, 'w') as f:
        f.write(TEMPLATE % (tn, tn, tn, tn))


def generate_haskell_requests_from_java():
    """ Copy all the request objects from the java SDK """
    files = get_java_files()
    for file_name in files:
        equivalent = file_name.replace("java", "hs")
        if not os.path.isfile(equivalent):
            generate_file(equivalent)

def main():
    generate_haskell_requests_from_java()

if __name__ == '__main__':
    main()
