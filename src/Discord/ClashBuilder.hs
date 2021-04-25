{-# LANGUAGE OverloadedStrings #-}
module Discord.ClashBuilder (BuilderMode(..), GameOptionBuilder(..), expandSharps, sendInviteEmbed, requestGame, sendBuilderEmbed, promptLanguages, promptModes, promptSubmit, promptModesByReaction, promptLangByReaction) where


import Data.IORef
import qualified Data.Map as M
import Discord.Types
import Clash.Game
import Discord
import Discord.Internal.Types
import qualified Discord.Requests as R
import qualified Data.Text as T
import Data.Either
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Concurrent
import Control.Monad.Trans.Maybe (MaybeT (..))
import Clash.Auth


data BuilderMode = GameMode | GameLang deriving (Show, Eq)

data GameOptionBuilder = GameOptionBuilder {
    creator :: UserId,
    options :: GameOptions,
    mode :: BuilderMode
} deriving Show

sendBuilderEmbed :: GameOptionBuilder -> ChannelId -> DiscordHandler (Either RestCallErrorCode Message)
sendBuilderEmbed builder channel = do
    result <- restCall $ R.CreateMessageEmbed channel "" $
            def {   createEmbedTitle = "Clash Of Code"
                ,   createEmbedThumbnail = Just $ CreateEmbedImageUrl
                    "https://files.codingame.com/codingame/share_pics_clash_of_code.jpg"
                ,   createEmbedDescription = description $ mode builder
                }
    _ <- either (\x -> pure ()) (createBuilderReactions (mode builder) channel) result
    pure result

description :: BuilderMode -> T.Text
description GameMode = "Please select allowed game modes!"
description GameLang = "Please select allowed languages!"

createBuilderReactions :: BuilderMode -> ChannelId -> Message -> DiscordHandler ()
createBuilderReactions GameMode cId msg = do
    a <- executeFor (sendReaction cId $ messageId msg) $ M.elems promptModes
    b <- sendReaction cId (messageId msg) promptSubmit
    liftIO $ print a
    pure ()
createBuilderReactions GameLang cId msg = do
    a <- executeFor (sendReaction cId $ messageId msg) $ M.elems promptLanguages
    b <- sendReaction cId (messageId msg) promptSubmit
    liftIO $ print a
    pure ()

executeFor :: (T.Text -> DiscordHandler ()) -> [T.Text] -> DiscordHandler ()
executeFor f (x : xs) = do
    _ <- f x
    executeFor f xs
executeFor f _ = pure ()

sendReaction :: ChannelId -> MessageId -> T.Text -> DiscordHandler ()
sendReaction channelId messageId emoteId = do
        _ <- restCall $ R.CreateReaction (channelId , messageId) emoteId
        pure ()

promptLanguages :: M.Map T.Text T.Text
promptLanguages = M.fromList
    [
        ("Haskell", "<:Haskell:834798923801296929>"),
        ("Kotlin", "<:Kotlin:834798924157288488>"),
        ("Javascript", "<:Javascript:834798924208406538>"),
        ("Java", "<:Java:834798924207489027>"),
        ("F#", "<:FSharp:834798923746115614>"),
        ("Go", "<:Go:834798923750047804>"),
        ("C#", "<:CSharp:834798923783995466>"),
        ("C++", "<:Cpp:834798923863556096>"),
        ("Clojure", "<:Clojure:834798923285135471>")
    ]
expandSharps :: T.Text  -> T.Text 
expandSharps txt = case txt of
    "C#" -> "CSharp"
    "F#" -> "FSharp"
    _ -> txt

promptLangByReaction :: M.Map T.Text T.Text
promptLangByReaction = invertMap promptLanguages

promptModes :: M.Map T.Text T.Text
promptModes = M.fromList
    [
        ("Shortest", "🇸"),
        ("Fastest", "🇫"),
        ("Reverse", "🇷")
    ]

promptModesByReaction :: M.Map T.Text T.Text
promptModesByReaction = invertMap promptModes

promptSubmit :: T.Text
promptSubmit = "✅"

invertMap :: Ord b => M.Map a b ->  M.Map b a
invertMap map = M.fromList [(v, k) | (k, v) <- M.toList map]

requestGame :: [T.Text] -> [T.Text] -> MaybeT IO GameInfo
requestGame modes langs = do
    email <- liftIO envEmail
    password <- liftIO envPassword
    session <- createSession $ Credentials { email = T.pack email , password = T.pack password }
    createGame GameOptions { modes = modes, languages = langs } session


sendInviteEmbed :: ChannelId -> Maybe GameInfo -> DiscordHandler ()
sendInviteEmbed channel info = do
    _ <- restCall $ R.CreateMessageEmbed channel "" $
                    def {   createEmbedTitle = "Clash Of Code"
                        ,   createEmbedThumbnail = Just $ CreateEmbedImageUrl
                            "https://files.codingame.com/codingame/share_pics_clash_of_code.jpg"
                        ,   createEmbedDescription = maybe "Error creating game... Please try again later!" createInviteMessage info
                        }
    pure ()

createInviteMessage :: GameInfo -> T.Text
createInviteMessage info =
    "You have been invited to a round of Clash Of Code!\n" <>
    "[Click here to Join!](" <> (handleLink . handle) info <> ")"