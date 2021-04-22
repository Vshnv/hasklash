{-# LANGUAGE OverloadedStrings #-}
module Discord.ClashBuilder (BuilderMode(..), GameOptionBuilder(..), sendBuilderEmbed, promptLanguages, promptModes, promptSubmit) where


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

data BuilderMode = GameMode | GameLang deriving Show

data GameOptionBuilder = GameOptionBuilder {
    creator :: UserId,
    options :: GameOptions,
    mode :: BuilderMode
} deriving Show

sendBuilderEmbed :: GameOptionBuilder -> ChannelId -> DiscordHandler ()
sendBuilderEmbed builder channel = do
    result <- restCall $ R.CreateMessageEmbed channel "" $
            def {   createEmbedTitle = "Clash Of Code"
                ,   createEmbedThumbnail = Just $ CreateEmbedImageUrl
                    "https://files.codingame.com/codingame/share_pics_clash_of_code.jpg"
                ,   createEmbedDescription = description $ mode builder
                }
    either (\x -> pure ()) (createBuilderReactions (mode builder) channel) result

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
        ("FSharp", "<:FSharp:834798923746115614>"),
        ("Go", "<:Go:834798923750047804>"),
        ("CSharp", "<:CSharp:834798923783995466>"),
        ("Cpp", "<:Cpp:834798923863556096>"),
        ("Clojure", "<:Clojure:834798923285135471>")
    ]

promptModes :: M.Map T.Text T.Text
promptModes = M.fromList 
    [
        ("Shortest", "🇸"),
        ("Fastest", "🇫"),
        ("Reverse", "🇷")
    ]

promptSubmit :: T.Text 
promptSubmit = "✅"