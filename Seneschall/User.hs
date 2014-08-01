module Seneschall.Pages.User where

import qualified Data.Text.Lazy as T
import qualified Text.Blaze.Html as H
import Text.Blaze.Html ((!))

import Seneschall.Database
import Seneschall.Types

loginPage :: Maybe T.Text -> ReqM ()
loginPage e = do
    return ()

login :: ReqM ()
login = do
    return ()

