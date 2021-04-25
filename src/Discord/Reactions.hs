{-# LANGUAGE MultiWayIf #-}

module Discord.Reactions where

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Discord.ClashBuilder as B
import qualified Data.Map as M
import Clash.Game
import Data.List
import UnliftIO
import Data.Maybe
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.Text as T

trackGameReactionAdd :: IORef (M.Map MessageId GameOptionBuilder) -> ReactionInfo -> DiscordHandler ()
trackGameReactionAdd state = trackGameReactions state 
        (\option reaction -> GameOptions {
                languages = languages option,
                modes = maybe (modes option) (\name -> nub (name : modes option)) $ M.lookup reaction promptModesByReaction
        }) 
        (\option reaction-> GameOptions {
                languages = nub (reaction : languages option),
                modes = modes option
        })

trackGameReactionRemove :: IORef (M.Map MessageId GameOptionBuilder) -> ReactionInfo -> DiscordHandler ()
trackGameReactionRemove state = trackGameReactions state
    (\option reaction -> GameOptions {
            languages = languages option,
            modes = delete reaction $ modes option
    })
    (\option reaction -> GameOptions {
            languages = delete reaction $ languages option,
            modes = modes option
    })


trackGameSubmit :: IORef (M.Map MessageId GameOptionBuilder) -> ReactionInfo -> DiscordHandler ()
trackGameSubmit state info = do
    let reaction = emojiName $ reactionEmoji info
    if reaction == promptSubmit then do
        let mId = reactionMessageId info
        activeBuilders <- liftIO $ readIORef state
        let maybeBuilder = M.lookup mId activeBuilders
        case maybeBuilder of
            Just builder ->
                if creator builder /= reactionUserId info then pure ()
                else handleSubmission (mode builder) state mId builder info
            Nothing -> pure ()
    else pure ()

handleSubmission :: BuilderMode -> IORef (M.Map MessageId GameOptionBuilder) -> MessageId -> GameOptionBuilder -> ReactionInfo -> DiscordHandler ()
handleSubmission GameMode state mId builder info  = do
    let builderMod = GameOptionBuilder {
        creator = creator builder,
        B.options = B.options builder,
        mode = GameLang
    }
    _ <- restCall $ R.DeleteMessage (reactionChannelId info, reactionMessageId info)
    msgEither <- sendBuilderEmbed builderMod (reactionChannelId info)
    either
        (const $ pure ())
        (\message -> do
            let msgKey = messageId message
            _ <- liftIO $ modifyIORef state (M.insert msgKey builderMod . M.delete mId)
            pure ()
        )
        msgEither
    pure ()
handleSubmission GameLang state mId builder info = do
    let opt = B.options builder
    _ <- restCall $ R.DeleteMessage (reactionChannelId info, reactionMessageId info)
    gameInfo <- liftIO $ runMaybeT $ requestGame (modes opt) (languages opt)
    liftIO $ print gameInfo
    _ <- sendInviteEmbed (reactionChannelId info) gameInfo
    pure ()


trackGameReactions :: IORef (M.Map MessageId GameOptionBuilder) -> (GameOptions -> T.Text -> GameOptions) -> (GameOptions  -> T.Text -> GameOptions) -> ReactionInfo -> DiscordHandler ()
trackGameReactions state optionsModMode optionsModLang info =
    liftIO $ modifyIORef state
        (M.adjust
                (\builder ->
                    let reaction = emojiName $ reactionEmoji info
                        selectionMode = mode builder
                        selector = creator builder
                        modeSelect = selectionMode == GameMode
                        langSelect = selectionMode == GameLang
                    in if
                    | reactionUserId info /= selector -> builder
                    | reaction == promptSubmit -> builder
                    | modeSelect && elem reaction (M.elems promptModes) -> GameOptionBuilder {
                            creator = selector,
                            B.options = optionsModMode (B.options builder) reaction,
                            mode = selectionMode
                        }
                    | langSelect && elem reaction (map expandSharps $ M.keys promptLanguages) -> GameOptionBuilder {
                            creator = selector,
                            B.options = optionsModLang (B.options builder) reaction,
                            mode = selectionMode
                        }
                    | otherwise -> builder
                )
                mId
        )
    where mId = reactionMessageId info
