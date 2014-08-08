{-# LANGUAGE OverloadedStrings #-}

module Seneschall.Pages.Article where

import Control.Applicative ((<$>))
import Control.Monad.State (get)
import Control.Monad.Trans (lift)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Seneschall.Pages
import Seneschall.Types

create :: ReqM ()
create = template $ "ToDo"

createPage :: ReqM ()
createPage = template $ "ToDo"

list :: ReqM ()
list = template $ "ToDo"
