{-# LANGUAGE OverloadedStrings #-}

module Seneschall.Pages.Admin where


import Control.Applicative ((<$>))
import Control.Monad.State (get)
import Control.Monad.Trans (lift)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Seneschall.Pages
import Seneschall.Types

admin :: ReqM ()
admin = template $ "ToDo"
