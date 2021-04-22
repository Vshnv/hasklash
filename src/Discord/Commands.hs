{-# LANGUAGE OverloadedStrings #-}
module Discord.Commands (cmdClash) where

import Discord
import Discord.Types
import Data.Text as T
import qualified Discord.Requests as R
import Discord.Arguments
import Clash.Game
import Clash.Auth
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Maybe (MaybeT (..))
import Discord.ClashBuilder
import Data.IORef
import Data.Map


cmdClash :: [Text] -> IORef (Map MessageId GameOptionBuilder)  -> Message -> DiscordHandler ()
cmdClash args state msg =
    let quick = lookupFlag args "quick" "--"
        modes = lookupArgument args "m" "-"
        langs = lookupArgument args "l" "-"
    in
        if quick then do
            info <- liftIO $ runMaybeT $ requestGame modes langs
            sendInviteEmbed (messageChannel msg) info
        else do
            let builder = GameOptionBuilder { 
                creator = Discord.Types.userId $ messageAuthor msg,  
                Discord.ClashBuilder.options = GameOptions modes langs,
                mode = GameMode
            }
            sendBuilderEmbed builder $ messageChannel msg
            pure ()

requestGame :: [Text] -> [Text] -> MaybeT IO GameInfo
requestGame modes langs = do
    email <- liftIO envEmail
    password <- liftIO envPassword
    session <- createSession $ Credentials { email = T.pack email , password = T.pack password }
    createGame GameOptions { modes = modes, languages = langs } session


createInviteMessage :: GameInfo -> Text
createInviteMessage info = 
    "You have been invited to a round of Clash Of Code!\n" <>
    "[Click here to Join!](" <> (handleLink . handle) info <> ")"

sendInviteEmbed :: ChannelId -> Maybe GameInfo -> DiscordHandler ()
sendInviteEmbed channel info = do
    _ <- restCall $ R.CreateMessageEmbed channel "" $
                    def {   createEmbedTitle = "Clash Of Code"
                        ,   createEmbedThumbnail = Just $ CreateEmbedImageUrl
                            "https://files.codingame.com/codingame/share_pics_clash_of_code.jpg"
                        ,   createEmbedDescription = maybe "Error creating game... Please try again later!" createInviteMessage info
                        }
    pure ()
    