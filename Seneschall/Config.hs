module Seneschall.Config where

import Control.Monad (liftM)
import Data.Aeson
import qualified Data.ByteString.Lazy as B

import Seneschall.Types

readConfig :: String -> IO (Either String Config)
readConfig file = liftM eitherDecode $ B.readFile file
