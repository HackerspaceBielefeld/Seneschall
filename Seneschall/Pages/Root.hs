{-# LANGUAGE OverloadedStrings #-}

module Seneschall.Pages.Root where

import Seneschall.Pages (template)
import Seneschall.Types

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty.Trans (html)

root :: ReqM ()
root = html $ renderHtml $ template $ do
    H.div ! A.class_ "centerBox" $ loginForm

loginForm :: H.Html
loginForm = H.form ! A.action "/login" ! A.method "POST" $ do
    H.table $ do
        H.tr $ do
            H.td "Username: "
            H.td $ H.input ! A.name "user" ! A.type_ "text" ! A.size "30"
        H.tr $ do
            H.td "Password: "
            H.td $ H.input ! A.name "pass" ! A.type_ "password" ! A.size "30"
    H.input ! A.type_ "submit" ! A.value "login"
