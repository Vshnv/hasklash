module Discord.Reactions where

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Discord.ClashBuilder
import qualified Data.Map as M
import Clash.Game
import Data.List
import UnliftIO
import Data.Maybe
import Control.Monad.Trans.Maybe (MaybeT (..))


reactGameOptions :: IORef (M.Map MessageId GameOptionBuilder) -> ReactionInfo -> DiscordHandler ()
reactGameOptions state info = withReactionAuthor info $ \author ->
    let react = emojiName $ reactionEmoji info
    in if react == promptSubmit && (not . userIsBot) author then do
        let mid = reactionMessageId info
        requestMap <- liftIO $ readIORef state
        case M.lookup mid requestMap of
            Just res -> do
                _ <- withMessage info $ handleOption res state requestMap info
                pure ()
            Nothing  -> pure ()
        pure ()
    else pure ()

withMessage :: ReactionInfo -> (Message -> DiscordHandler ()) -> DiscordHandler ()
withMessage info func = do
    msgEither <- restCall $ R.GetChannelMessage (reactionChannelId info, reactionMessageId info)
    either (const $ pure ()) func msgEither
    pure ()

withReactionAuthor :: ReactionInfo -> (User -> DiscordHandler ()) -> DiscordHandler ()
withReactionAuthor info func = do
    reactionAuthor <- restCall $ R.GetUser (reactionUserId info)
    either (const $ pure ()) func reactionAuthor
    pure ()

handleOption :: GameOptionBuilder -> IORef (M.Map MessageId GameOptionBuilder) -> M.Map MessageId GameOptionBuilder -> ReactionInfo -> Message -> DiscordHandler ()
handleOption res state requestMap info msg =
    let reactions = messageReactions msg
        in
    case mode res of
        GameMode -> do
            let reactedModes = mapMaybe ((`M.lookup` promptModesByReaction) . (emojiName . messageReactionEmoji)) reactions
            _ <- restCall $ R.DeleteMessage (reactionChannelId info, reactionMessageId info)
            let gameOptions = Discord.ClashBuilder.options res
            let mutatedOptions = GameOptionBuilder (creator res) (GameOptions (modes gameOptions ++ reactedModes) (languages gameOptions)) GameLang
            liftIO $ print $ map (emojiName . messageReactionEmoji) reactions
            liftIO $ print $ M.keys promptModesByReaction 
            nmsgEither <- sendBuilderEmbed mutatedOptions (messageChannel msg)
            either (const $ pure ()) (\nmsg -> do
                _ <- liftIO $modifyIORef state (\stateMap ->
                    let remaining = M.delete (messageId msg) stateMap
                        in
                    M.insert (messageId nmsg) mutatedOptions remaining
                    )
                pure ()
                ) nmsgEither
            pure ()
        GameLang -> do
            let reactedLangs = mapMaybe ((`M.lookup` promptLangByReaction) . (emojiName . messageReactionEmoji)) reactions
            _ <- restCall $ R.DeleteMessage (reactionChannelId info, reactionMessageId info)
            let gameOptions = Discord.ClashBuilder.options res
            let modGameOptions = GameOptions (modes gameOptions) (languages gameOptions ++ reactedLangs)
            _ <- liftIO $ modifyIORef state $ M.delete (messageId msg)
            gameInfo <- liftIO $ runMaybeT $ requestGame (modes modGameOptions) (languages modGameOptions)
            _ <- sendInviteEmbed (messageChannel msg) gameInfo
            pure ()
 