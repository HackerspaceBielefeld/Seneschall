module Main where

import Crypto.Scrypt
import Data.ByteString.Char8 (pack)
import Data.Maybe (catMaybes)
import Database.Persist.Postgresql
import Seneschall.Config (readConfig)
import Seneschall.Database
import Seneschall.Types
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("adduser":u:p:r) -> addUser u p r
        ("initdb":_)      -> initDb
        _                 -> printUsage

printUsage :: IO ()
printUsage = do
    putStrLn "seneadmin [command] ..."
    putStrLn "  adduser [username] [password] roles"
    putStrLn "  initdb"

addUser :: String -> String -> [String] -> IO ()
addUser u p r = do
    conf <- readConfig "seneschall.conf"
    case conf of
         Left  e -> putStrLn $ "config error: " ++ e
         Right c -> do
             enc <- encryptPassIO defaultParams (Pass $ pack p)
             let dbString = dbFromConfig c
             withPostgresqlConn (pack dbString) $ runSqlPersistM (do
                 u' <- insert $ WebUser u "" (getEncryptedPass enc) UserActive
                 r' <- mapM (\a -> selectFirst [RoleName ==. a] []) r
                 mapM_ (insert . UserRole u' . entityKey) $ catMaybes r'
                 )
             putStrLn "user added"

initDb :: IO ()
initDb = do
    conf <- readConfig "seneschall.conf"
    case conf of
         Left  e -> putStrLn $ "config error: " ++ e
         Right c -> do
             let dbString = dbFromConfig c
             withPostgresqlConn (pack dbString) $ runSqlPersistM
                $ runMigration migrateAll
             print "db initialized"
