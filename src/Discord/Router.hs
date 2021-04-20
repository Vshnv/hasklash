{-# LANGUAGE OverloadedStrings #-}

module Discord.Router where


import Control.Monad (when)
import Data.Text (isPrefixOf, toLower, Text)
import Data.Maybe
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Control.Concurrent
import UnliftIO

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Data.Map as M

type CommandRouteMap = 
    Map Text ([Text] -> Message -> DiscordHandler ())

commandRouter :: Char -> CommandRouteMap -> Event -> DiscordHandler ()
commandRouter prefix routes (MessageCreate msg)
    | fromUser msg && hasCommandPrefix prefix (T.uncons $ messageText msg) =
        let msgText = messageText msg
            commandMeta = T.words $ T.tail msgText
            route = head commandMeta
            args = tail commandMeta
        in  fromMaybe (pure ())  (M.lookup route routes >>= (\route -> return $ route args msg))            
    | otherwise = return ()

hasCommandPrefix :: Char -> Maybe (Char, Text) -> Bool
hasCommandPrefix prefix (Just (mPrefix, _)) = prefix == mPrefix
hasCommandPrefix prefix _ = False

fromUser :: Message -> Bool
fromUser = not . fromUser

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)
