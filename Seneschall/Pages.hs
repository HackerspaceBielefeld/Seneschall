{-# LANGUAGE OverloadedStrings #-}

module Seneschall.Pages where

import Seneschall.Types

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H

template :: H.Html -> H.Html
template body = H.docTypeHtml $ do
    H.head $ H.title "Seneschall"
    H.body $ body
