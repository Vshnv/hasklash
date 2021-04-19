{-# LANGUAGE OverloadedStrings #-}
module Clash.Game where
import Control.Monad.IO.Class

import Data.Aeson.Types (Parser (..), parseMaybe)
import Data.Aeson ( Value(Object), decodeStrict, (.:), Object )
import Data.List (intercalate)
import Network.HTTP.Req
import qualified Data.ByteString as BS
import Data.ByteString.Char8 as C8 (pack)
import Clash.Auth
import Clash
import Lib ( unwrapObject )
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Functor ( (<&>) )

data GameOptions = GameOptions {
    languages :: [String],
    modes :: [String]
} deriving Show

data GameInfo = GameInfo {
    handle :: String,
    options :: GameOptions
} deriving Show


createGame :: GameOptions -> SessionToken -> MaybeT IO GameInfo
createGame info token = 
    let (GameOptions languages modes) = info 
        in
    MaybeT $ sendGameCreationRequest languages modes token <&> flip parseGameInfo info

sendGameCreationRequest :: MonadIO m => [String] -> [String] -> SessionToken -> m (JsonResponse Value)
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
        sessionCookie = C8.pack $ intercalate ";" $ cookie token

parseGameInfo :: JsonResponse Value -> GameOptions -> Maybe GameInfo
parseGameInfo response =
       (<*>) (GameInfo <$> handle) . Just
    where
        handle = parseMaybe gameHandleParser (unwrapObject $ responseBody response)


gameHandleParser :: Object -> Parser String
gameHandleParser = (.: "publicHandle")