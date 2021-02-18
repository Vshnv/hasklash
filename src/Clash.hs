-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Clash
        ( login
        , LoginResult
        , createPrivateGame
        , handleToLink
        ) where

import           Data.Aeson.Types      (parseMaybe, (.:), Value, Value(Number), Value(String), toJSON)
import           Data.Aeson            (Value(Object), Object)
import qualified Data.ByteString.Char8 as S8
import           Network.HTTP.Simple
import           Control.Monad.IO.Class
import           Data.Scientific (toBoundedInteger , Scientific)
import           Data.ByteString.Char8 (intercalate)
import           Control.Monad ((>=>))
import qualified Data.Text as T

data LoginResult =
     LoginResult
     { cookieStr :: S8.ByteString
     , userId :: Int
     } deriving (Show)

base :: S8.ByteString
base = "codingame.com"

setBasicHeaders :: Request -> Request
setBasicHeaders continuation
              = setRequestHeader "accept" ["application/json, text/plain, */*"]
              $ setRequestHeader "accept-language" ["en-US,en;q=0.9"]
              $ setRequestHeader "sec-ch-ua" ["\"Google Chrome\";v=\"87\", \" Not;A Brand\";v=\"99\", \"Chromium\";v=\"87\""]
              $ setRequestHeader "sec-ch-ua-mobile" ["?0"]
              $ setRequestHeader "sec-fetch-dest" ["empty"]
              $ setRequestHeader "sec-fetch-mode" ["cors"]
              $ setRequestHeader "sec-fetch-site" ["same-origin"]
              $ setRequestHeader "refferer" ["https://www.codingame.com/multiplayer/clashofcode"]
              $ setRequestHeader "reffererPolicy" ["strict-origin-when-cross-origin"] continuation



login :: (MonadIO m) => String -> String -> m (Maybe LoginResult)
login email password = do
            let request
                  = setRequestPath "/services/CodinGamer/loginSiteV2"
                  $ setRequestMethod "POST"
                  $ setRequestHost base
                  $ setRequestBodyJSON [email, password, "true"]
                  $ setBasicHeaders defaultRequest
            resp <- httpJSON request
            return (parseLoginResponse resp)

parseLoginResponse :: Response Value -> Maybe LoginResult
parseLoginResponse resp = parseMaybe (.: "codinGamer") (unwrapObject $ getResponseBody resp)
                        >>= parseMaybe (.: "userId")
                        >>= (toBoundedInteger . unwrapNumber 
                            >=> (Just . LoginResult (intercalate ";" $ getResponseHeader "set-cookie" resp)) )
                            


createPrivateGame :: MonadIO m => [String] -> [String] -> LoginResult -> m (Maybe String)
createPrivateGame lang modes auth = do
            let request
                  = setRequestPath "/services/ClashOfCode/createPrivateClash"
                  $ setRequestMethod "POST"
                  $ setRequestHost base
                  $ setRequestBodyJSON (toJSON (userId auth, [("SHORT"::String, True)], lang, modes))
                  $ setRequestHeader "cookie" [cookieStr auth]
                  $ setBasicHeaders defaultRequest
            resp <- httpJSON request
            return $ parseGameHandle resp

parseGameHandle :: Response Value -> Maybe String
parseGameHandle resp = parseMaybe (.: "publicHandle") (unwrapObject $ getResponseBody resp)
                     >>= (Just . unwrapString)

handleToLink :: String -> String
handleToLink handle = "https://www.codingame.com/clashofcode/clash/" ++ handle

unwrapObject :: Value -> Object
unwrapObject (Object x) = x
unwrapObject _ = error "No Object available"

unwrapNumber :: Value -> Data.Scientific.Scientific
unwrapNumber (Number x) = x
unwrapNumber _ = error "No Number available"

unwrapString :: Value -> String
unwrapString (String x) = T.unpack x
unwrapString _ = error "No Number available"