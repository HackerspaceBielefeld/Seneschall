{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Seneschall.Session 
    ( createSessionStore
    , createSession
    , lookupSession
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.Acid
import Data.Acid.Memory
import qualified Data.Map.Strict as M
import System.Random

import Seneschall.Types

insert :: Key -> Session -> Update SessionStore ()
insert k v = do
    SessionStore m <- get
    put $ SessionStore (M.insert k v m)

lookup :: Key -> Query SessionStore (Maybe Session)
lookup k = do
    SessionStore m <- ask
    return $ M.lookup k m

toList :: Query SessionStore [Session]
toList = do
    SessionStore m <- ask
    return $ map snd $ M.toList m

fromList :: [Session] -> Update SessionStore ()
fromList s = put $ SessionStore $ M.fromList $ map (\a -> (key a, a)) s

count :: Query SessionStore Int
count = do
    SessionStore m <- ask
    return $ M.size m

$(makeAcidic ''SessionStore ['insert, 'lookup, 'toList, 'fromList, 'count])

createSessionStore :: IO (AcidState SessionStore)
createSessionStore = openMemoryState $ SessionStore M.empty

keyChars :: String
keyChars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

genKey :: IO String
genKey = mapM (const $ liftM (keyChars !!) $ randomRIO (0,61)) [1 :: Int ..64]

createSession :: Int -> WebM String
createSession u = do
    k <- liftIO genKey
    s <- session <$> ask
    liftIO $ update s (Insert k (Session k u))
    return k

lookupSession :: String -> WebM (Maybe Session)
lookupSession k = do
    s <- session <$> ask
    liftIO $ query s (Lookup k)

