module Discord.Reactions where

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Discord.ClashBuilder
import qualified Data.Map as M 
import UnliftIO

reactGameOptions :: IORef (M.Map MessageId GameOptionBuilder) -> ReactionInfo -> DiscordHandler ()
reactGameOptions state info = 
    let react = emojiName $ reactionEmoji info
    in if react == promptSubmit then do
        liftIO $ print react
        let mid = reactionMessageId info
        requestMap <- liftIO $ readIORef state
        case M.lookup mid requestMap of
            Just res -> do
                _ <- handleOption res state requestMap info
                pure ()
            Nothing  -> pure ()
        pure ()
    else pure ()

handleOption :: GameOptionBuilder -> IORef (M.Map MessageId GameOptionBuilder) -> M.Map MessageId GameOptionBuilder -> ReactionInfo  -> DiscordHandler ()
handleOption res state requestMap info = 
    case mode res of
        GameMode -> do
            msgEither <- restCall $ R.GetChannelMessage ((reactionChannelId info, reactionMessageId info)) 
            if isRight msgEither then do
                let msg = right msgEither
                let reactions = messageReactions msg
                
                pure ()
            else pure ()
            pure ()
        GameLang -> do

            pure ()
 