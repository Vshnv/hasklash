{-# LANGUAGE OverloadedStrings #-}


{-# LANGUAGE DataKinds #-}
module Clash.Auth (createSession, envEmail, envPassword, Credentials (..), SessionToken (..)) where


import Control.Monad
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Maybe (MaybeT (..))
import Clash ( defaultHeaders )
import Data.Aeson ( Value(Object, Number), decodeStrict, (.:), Object)
import Data.Aeson.Types (Parser (..), parseMaybe)
import Data.Functor ( (<&>) )
import Lib ( unwrapObject )
import Data.Scientific (toBoundedInteger , Scientific)
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
      responseHeader,
      responseCookieJar)
import Network.HTTP.Client (CookieJar)
import System.Environment
import qualified Data.Text as T
import Data.Maybe
import Data.ByteString
import qualified Data.Text.Encoding as TEncode



data Credentials = Credentials {
     email :: T.Text,
     password :: T.Text
} deriving Show

data SessionToken = SessionToken {
    userId :: T.Text,
    cookie :: CookieJar
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
parseLoginResponse response = do
        userId >>= (\x -> Just $ x sessionCookies) . SessionToken
    where
        userId = (parseMaybe userIdParser <=< parseMaybe profileParser $ unwrapObject $ responseBody response) >>= (toBoundedInteger :: Scientific -> Maybe Int) <&> T.pack . show
        sessionCookies = responseCookieJar response

profileParser :: Object  -> Parser Object
profileParser = (.: "codinGamer")

userIdParser :: Object -> Parser Scientific
userIdParser = (.: "userId")

fetchSessionCookies :: JsonResponse Value -> Maybe ByteString
fetchSessionCookies response = responseHeader response "set-cookie"

envEmail :: IO String
envEmail = getEnv "EMAIL"

envPassword :: IO String
envPassword = getEnv "PASS"
