{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}

module Seneschall.Types where

import Prelude hiding (lookup)

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State  as S
import Data.Acid
import Data.Aeson
import qualified Data.Map.Strict as M
import Data.SafeCopy
import Data.Text.Lazy (Text)
import Data.Typeable
import Database.Persist.Sql (ConnectionPool)
import Database.Persist.TH (derivePersistField)
import Web.Scotty.Trans (ScottyT, ActionT)

-- session key is a string (will be stored in a cookie)
type Key = String
-- information stored in a user session
data Session = Session {
    key  :: Key,
    user :: Int
} deriving (Typeable, Show)
$(deriveSafeCopy 0 'base ''Session)

-- Data type for storing sessions. Made acidic by Seneschall.Session.
data SessionStore = SessionStore (M.Map Key Session) deriving (Typeable)
$(deriveSafeCopy 0 'base ''SessionStore)

-- configuration
data Config = Config {
    port   :: Int,
    dbHost :: String,
    dbName :: String,
    dbUser :: String,
    dbPass :: String,
    dbPort :: Int,
    htdoc  :: String
} deriving (Show)
instance FromJSON Config where
    parseJSON (Object v) = Config <$>
                           v .: "port"   <*>
                           v .: "dbHost" <*>
                           v .: "dbName" <*>
                           v .: "dbUser" <*>
                           v .: "dbPass" <*>
                           v .: "dbPort" <*>
                           v .: "htdoc"
    parseJSON _          = mzero

-- global values for the reader monad
data Globals = Globals {
    config  :: Config,
    session :: AcidState SessionStore,
    db      :: ConnectionPool
}

-- values per request for the state monad
data ReqState = ReqState {
    userName :: Maybe String,
    roles    :: [String]
}

data UserStatus = UserActive | UserInactive deriving (Show, Read, Eq)
derivePersistField "UserStatus"

newtype WebM a = WebM {runWebM :: R.ReaderT Globals (S.StateT ReqState IO) a}
    deriving ( Functor, Applicative, Monad , MonadIO
             , R.MonadReader Globals, S.MonadState ReqState)
-- application monad
type AppM = ScottyT Text WebM
-- request monad
type ReqM = ActionT Text WebM

