{-# LANGUAGE OverloadedStrings #-}

module Seneschall.Pages.Article where

import Control.Applicative ((<$>))
import Control.Monad.Trans (lift)
import Data.Foldable (foldl')
import qualified Data.Text.Lazy as T
import Database.Persist.Postgresql
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty.Trans

import Seneschall.Database
import Seneschall.Pages
import Seneschall.Types

create :: ReqM ()
create = do
    name <- param "name"
    ean  <- param "ean"
    _ <- lift $ runSql $ insert $ Article name ean
    template "ToDo"

createPage :: ReqM ()
createPage = template $ createHtml Nothing

createHtml :: Maybe T.Text -> H.Html
createHtml e = do
  case e of
      Nothing -> return ()
      Just e' -> H.div ! A.class_ "error" $ H.toHtml e'
  H.form ! A.action "/article/create" ! A.method "POST" $ do
    H.span ! A.class_ "lable" $ "name"
    H.input ! A.type_ "text" ! A.name "name" ! A.size "30"
    H.span ! A.class_ "lable" $ "ean"
    H.input ! A.type_ "text" ! A.name "ean" ! A.size "30"
    H.input ! A.type_ "submit" ! A.value "create Article"

list :: ReqM ()
list = do
    as <- lift $ map entityVal <$> runSql (selectList [] [Asc ArticleName])
    template $ H.table $ foldl' (\h e -> h >> row e ) (return ()) as
    where
        row :: Article -> H.Html
        row a = H.tr $ do
            H.td $ H.toHtml $ articleName a
            H.td $ H.toHtml $ articleEan  a

