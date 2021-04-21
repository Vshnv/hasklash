{-# LANGUAGE OverloadedStrings #-}


module Clash.Auth (createSession, envEmail, envPassword, Credentials (..), SessionToken (..)) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Maybe (MaybeT (..))
import Clash ( defaultHeaders )
import Data.Aeson ( Value(Object), decodeStrict, (.:), Object )
import Data.Aeson.Types (Parser (..), parseMaybe)
import Data.Functor ( (<&>) )
import Lib ( unwrapObject )
import Network.HTTP.Req
    ( responseBody,
      jsonResponse,
      req,
      (/:),
      https,
      defaultHttpConfig,
      runReq,
      ReqBodyJson(ReqBodyJson),
      POST(POST),
      JsonResponse,
      responseHeader )
import System.Environment
import qualified Data.Text as T


data Credentials = Credentials {
     email :: T.Text,
     password :: T.Text
} deriving Show

data SessionToken = SessionToken {
    userId :: T.Text,
    cookie :: [T.Text]
} deriving Show


createSession :: MonadIO m => Credentials -> MaybeT m SessionToken
createSession credentials = MaybeT $ sendLoginRequest credentials <&> parseLoginResponse


sendLoginRequest :: MonadIO m => Credentials -> m (JsonResponse Value)
sendLoginRequest (Credentials email password) = runReq defaultHttpConfig $ do
    let loginPath = https "codingame.com" /: "services" /: "CodinGamer" /: "loginSiteV2"
    let payload = (email, password, True)
    r <- req POST loginPath (ReqBodyJson payload) jsonResponse defaultHeaders
    liftIO $ return r

parseLoginResponse :: JsonResponse Value -> Maybe SessionToken
parseLoginResponse response =
        SessionToken <$> userId <*> sessionCookies
    where
        userId = (parseMaybe userIdParser . unwrapObject $ responseBody response) <&> T.pack
        sessionCookies = fetchSessionCookies response <&> map T.pack

userIdParser :: Object  -> Parser String
userIdParser = (.: "codinGamer.userId")

fetchSessionCookies :: JsonResponse Value -> Maybe [String]
fetchSessionCookies response = responseHeader response "set-cookie" >>= decodeStrict

envEmail :: IO String
envEmail = getEnv "EMAIL"

envPassword :: IO String
envPassword = getEnv "PASS"
