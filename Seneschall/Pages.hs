{-# LANGUAGE OverloadedStrings #-}

module Seneschall.Pages where

import Seneschall.Types

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.State (get)
import Control.Monad.Trans (lift)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty.Trans (html)

template :: H.Html -> ReqM ()
template body = do
    rs <- lift $ roles <$> get
    html $ renderHtml $ H.docTypeHtml $ do
        H.head $ H.title "Seneschall"
        H.body $ do 
            menu rs
            H.div ! A.class_ "main" $ body

menu :: [Role] -> H.Html
menu rs = H.ul ! A.class_ "menu" $ do
    when (rs /= []) $ H.li $ H.a ! A.href "/logout" $ "logout"
    when (null rs ) $ H.li $ H.a ! A.href "/login"  $ "login"
