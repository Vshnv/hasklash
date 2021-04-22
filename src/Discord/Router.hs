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
import Discord.ClashBuilder

type CommandRouteMap =
    Map Text ([Text] -> IORef (Map MessageId GameOptionBuilder) -> Message -> DiscordHandler ())

type ReactionRouterList =
    [IORef (Map MessageId GameOptionBuilder) -> ReactionInfo -> DiscordHandler ()]

eventRouter :: (IORef (Map MessageId GameOptionBuilder) -> [Event -> DiscordHandler ()]) -> IORef (Map MessageId GameOptionBuilder) -> Event -> DiscordHandler ()
eventRouter eventListeners builderRef event = do
    let listeners = eventListeners builderRef
    applyEvent (eventListeners builderRef) event
    pure ()

applyEvent :: [Event -> DiscordHandler ()] -> Event -> DiscordHandler ()
applyEvent (route : routes) event = do
    _ <- route event
    applyEvent routes event
applyEvent [] _ = pure ()

commandRouter :: Char -> CommandRouteMap -> IORef (Map MessageId GameOptionBuilder) -> Event -> DiscordHandler ()
commandRouter prefix routes state (MessageCreate msg)
    | fromUser msg && hasCommandPrefix prefix (T.uncons $ messageText msg) =
        let msgText = messageText msg
            commandMeta = T.words $ T.tail msgText
            route = head commandMeta
            args = tail commandMeta
        in do
            fromJust $ M.lookup route routes >>= (\route -> return $ route args state msg)
            pure ()
    | otherwise = return ()
commandRouter _ _ _ _ = return ()

reactionRouter :: ReactionRouterList -> IORef (Map MessageId GameOptionBuilder) -> Event -> DiscordHandler ()
reactionRouter reactionRouters state (MessageReactionAdd reactInfo) = 
    Prelude.mapM_ (\x -> x state reactInfo) reactionRouters
reactionRouter _ _ _ = return ()

hasCommandPrefix :: Char -> Maybe (Char, Text) -> Bool
hasCommandPrefix prefix (Just (mPrefix, _)) = prefix == mPrefix
hasCommandPrefix prefix _ = False

fromUser :: Message -> Bool
fromUser = not . fromBot

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)
