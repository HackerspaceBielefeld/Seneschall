{-# LANGUAGE OverloadedStrings #-}
module Seneschall.Pages.User where

import Blaze.ByteString.Builder (toLazyByteString)
import qualified Control.Monad.State as State
import Control.Applicative ((<$>))
import Control.Monad.Trans.Class (lift)
import Crypto.Scrypt
import Data.ByteString.Char8 (pack)
import Data.Convertible (convert)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import qualified Text.Blaze.Html as H
import qualified Database.Persist.Postgresql as DB
import Database.Persist.Postgresql ((==.))
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Cookie
import Web.Scotty.Trans

import Seneschall.Database
import Seneschall.Pages (template)
import Seneschall.Session
import Seneschall.Types

loginPage :: Maybe T.Text -> ReqM ()
loginPage e = html $ renderHtml $ template $ do
    case e of
        Just e' -> H.div ! A.class_ "error" $ H.toHtml e'
        Nothing -> return ()
    H.table $ do
        H.tr $ do
            H.td "Username: "
            H.td $ H.input ! A.name "user" ! A.type_ "text" ! A.size "30"
        H.tr $ do
            H.td "Password: "
            H.td $ H.input ! A.name "pass" ! A.type_ "password" ! A.size "30"
    H.input ! A.type_ "submit" ! A.value "login"

login :: ReqM ()
login = do
    uKey <- testCredentials
    case uKey of
        Just k  -> createSessionCookie k >> redirect "/"
        Nothing -> loginPage $ Just "Falscher Benutzername oder Passwort"

logout :: ReqM ()
logout = deleteSessionCookie >> redirect "/"

createSessionCookie :: Int -> ReqM ()
createSessionCookie uKey = do
    sKey <- lift $ createSession uKey
    setHeader "Set-Cookie" (cookie sKey) where
        cookie :: String -> T.Text
        cookie sKey = decodeUtf8 . toLazyByteString . renderSetCookie
            $ def { setCookieName = "session", setCookieValue = pack sKey }

deleteSessionCookie :: ReqM ()
deleteSessionCookie = setHeader "Set-Cookie" cookie where
    cookie :: T.Text
    cookie = decodeUtf8 . toLazyByteString . renderSetCookie
        $ def { setCookieName = "session", setCookieValue = ""}


sessionFromCookie :: ReqM (Maybe Session)
sessionFromCookie = do
    h <- header "Cookie"
    let cs = fmap (parseCookiesText . convert . encodeUtf8) h
    case cs >>= (lookup "session") of
        Just c  -> lift $ lookupSession $ convert c
        Nothing -> return Nothing

userFromSession :: Session -> ReqM (Maybe ReqState)
userFromSession s = do
    let uKey   = user s
        userId = (keyFromIntegral uKey :: WebUserId)
    u  <- lift $ runSql $ DB.get userId
    r  <- lift $ runSql $ DB.selectList [UserRoleUserId ==. userId] []
    case u of
        Just u' -> return $ Just $ ReqState (Just $ webUserName u')
                   (map (userRoleRole . DB.entityVal) r)
        Nothing -> return $ Nothing

restoreSession :: ReqM ()
restoreSession = do
    s <- sessionFromCookie
    case s of
        Just s' -> do
            u <- userFromSession s'
            case u of
                Just u' -> do
                    lift $ State.put u'
                Nothing -> return ()
        Nothing -> return ()

requiredRole :: Role -> ReqM () -> ReqM ()
requiredRole r action = do
    rs <- lift $ roles <$> State.get
    if r `elem` rs then action else accessDenied

accessDenied :: ReqM ()
accessDenied = html $ renderHtml $ template $
    H.div ! A.class_ "error" $ "Access Denied"


testCredentials :: ReqM (Maybe Int)
testCredentials = do
    u <- param "user"
    p <- param "pass"
    reqRes <- lift $ runSql $ DB.selectFirst [WebUserName ==. u] []
    case  reqRes of
        Just (DB.Entity k' u') -> return $
            if verifyPass' (Pass p) (EncryptedPass (webUserPassword u'))
            then Just (keyToNum k') else Nothing
        Nothing -> return Nothing


