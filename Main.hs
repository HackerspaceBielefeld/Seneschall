{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.State  (evalStateT)
import Control.Monad.Trans.Class (lift)
import Seneschall.Config (readConfig)
import Seneschall.Database
import Seneschall.Session
import Seneschall.Types

import qualified Seneschall.Pages.Admin    as Admn
import qualified Seneschall.Pages.Article  as Arti
import qualified Seneschall.Pages.Purchase as Purr
import qualified Seneschall.Pages.Root     as Root
import qualified Seneschall.Pages.User     as User

import Network.Wai.Middleware.Static
import Web.Scotty.Trans

main :: IO ()
main = do
    putStrLn "ToDo"
    conf <- readConfig "seneschall.conf"
    case conf of
        Right c -> do
            g <- initGlobals c
            energise g dispatcher
        Left  e -> putStrLn $ "config error: " ++ e

dispatcher :: AppM ()
dispatcher = do
    conf <- lift $ config <$> ask
    middleware $ staticPolicy $ noDots >-> addBase (htdoc conf)
    get  "/"       $ User.restoreSession >> Root.root
    get  "/article/new"     $ User.requiredRole User $ Arti.createPage
    post "/article/create"  $ User.requiredRole User $ Arti.create
    get  "/article/list"    $ User.requiredRole User $ Arti.list
    get  "/purchase/new"    $ User.requiredRole User $ Purr.createPage
    post "/purchase/create" $ User.requiredRole User $ Purr.create
    get  "/purchase/list"   $ User.requiredRole User $ Purr.list
    get  "/login"  $ User.loginPage Nothing
    post "/login"  $ User.login
    get  "/logout" $ User.logout
    get  "/admin"  $ User.requiredRole Admin $ Admn.admin
    notFound $ text "you appear to be lost"

energise :: Globals -> AppM () -> IO ()
energise g app = do
    let p    = port $ config g
    scottyT p runM runM app
      where
        runM :: WebM a -> IO a
        runM a = evalStateT (runReaderT (runWebM a) g) (ReqState Nothing [])

initGlobals :: Config -> IO Globals
initGlobals conf = do
    pool  <- createConnectionPool conf
    store <- createSessionStore
    return $ Globals conf store pool
