{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Seneschall.Database where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString)
import qualified Data.Text.Lazy as T
import Data.Time.Calendar (Day)
import Data.Int
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

import Seneschall.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
WebUser
    name      T.Text
    email     T.Text
    password  ByteString
    status    UserStatus
    deriving  Show
UserRole
    userId    WebUserId
    role      Role
    deriving  Show
Currency
    name      T.Text
    symbol    T.Text
    precision Int
    deriving  Show
Article
    name      T.Text
    ean       T.Text
    deriving  Show
Purchase
    userId    WebUserId
    date      Day
    deriving  Show
PurchasedArticle
    purchase  PurchaseId
    article   ArticleId
    amount    Int
    price     Int64
    currency  CurrencyId
    deriving  Show
|]

dbFromConfig :: Config -> String
dbFromConfig c = "host="     ++       dbHost c
             ++ " port="     ++ show (dbPort c)
             ++ " user="     ++       dbUser c
             ++ " dbname="   ++       dbName c
             ++ " password=" ++       dbPass c

createConnectionPool :: Config -> IO ConnectionPool
createConnectionPool c =
    createPostgresqlPool (pack $ dbFromConfig c) 10

runSql :: SqlPersistM a -> WebM a
runSql sql = do
    conn <- db <$> ask
    liftIO $ runSqlPersistMPool sql conn

keyFromIntegral :: Integral i => i -> KeyBackend SqlBackend e
keyFromIntegral i = Key $ PersistInt64 $ fromIntegral i

keyToNum :: Num n => KeyBackend SqlBackend e -> n
keyToNum (Key (PersistInt64 n)) = fromIntegral n
keyToNum _                      = error "unexpected key in keyToNum"

