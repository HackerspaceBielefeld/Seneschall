{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}

module Seneschall.Types where

import Prelude hiding (lookup)

import Control.Applicative
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
import GHC.Generics
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
} deriving (Show, Generic)

instance FromJSON Config
instance ToJSON   Config

data UserStatus = UserActive | UserInactive deriving (Show, Read, Eq)
derivePersistField "UserStatus"

data Role = User | Admin deriving (Show, Read, Eq)
derivePersistField "Role"

-- global values for the reader monad
data Globals = Globals {
    config  :: Config,
    session :: AcidState SessionStore,
    db      :: ConnectionPool
}

-- values per request for the state monad
data ReqState = ReqState {
    userName :: Maybe String,
    roles    :: [Role]
}

newtype WebM a = WebM {runWebM :: R.ReaderT Globals (S.StateT ReqState IO) a}
    deriving ( Functor, Applicative, Monad , MonadIO
             , R.MonadReader Globals, S.MonadState ReqState)
-- application monad
type AppM = ScottyT Text WebM
-- request monad
type ReqM = ActionT Text WebM

