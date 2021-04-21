{-# LANGUAGE OverloadedStrings #-}


module Clash.Game (createGame, GameInfo (..), GameOptions (..)) where


import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Maybe (MaybeT (..))
import Clash.Auth ( SessionToken(userId, cookie) )
import Clash ( defaultHeaders )
import Data.Aeson.Types (Parser (..), parseMaybe)
import Data.Aeson ( Value(Object), decodeStrict, (.:), Object )
import qualified Data.ByteString as BS
import Data.ByteString.Char8 as C8 (pack)
import Data.Functor ( (<&>) )
import Data.List (intercalate)
import Network.HTTP.Req
    ( header,
      responseBody,
      jsonResponse,
      req,
      (/:),
      https,
      defaultHttpConfig,
      runReq,
      ReqBodyJson(ReqBodyJson),
      POST(POST),
      JsonResponse )
import qualified Data.Text as T
import Lib ( unwrapObject )
import qualified Data.Text.Encoding as TEncode

data GameOptions = GameOptions {
    languages :: [T.Text],
    modes :: [T.Text]
} deriving Show

data GameInfo = GameInfo {
    handle :: T.Text,
    options :: GameOptions
} deriving Show


createGame :: GameOptions -> SessionToken -> MaybeT IO GameInfo
createGame info token = 
    let (GameOptions languages modes) = info 
        in
    MaybeT $ sendGameCreationRequest languages modes token <&> flip parseGameInfo info

sendGameCreationRequest :: MonadIO m => [T.Text] -> [T.Text] -> SessionToken -> m (JsonResponse Value)
sendGameCreationRequest languages modes token = runReq defaultHttpConfig $
        req
            POST
            loginPath
            (ReqBodyJson payload)
            jsonResponse
            (defaultHeaders <> header "cookie" sessionCookie)
            >>= (liftIO . return)
    where
        loginPath = https "codingame.com" /: "services" /: "ClashOfCode" /: "createPrivateClash"
        payload = (userId token, [("SHORT"::String, True)], languages, modes)
        sessionCookie =  TEncode.encodeUtf8 $ T.intercalate ";" $ cookie token

parseGameInfo :: JsonResponse Value -> GameOptions -> Maybe GameInfo
parseGameInfo response =
       (<*>) (GameInfo <$> handle) . Just
    where
        handle = parseMaybe gameHandleParser (unwrapObject $ responseBody response) <&> T.pack


gameHandleParser :: Object -> Parser String
gameHandleParser = (.: "publicHandle")