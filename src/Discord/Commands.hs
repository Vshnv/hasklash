{-# LANGUAGE OverloadedStrings #-}
module Discord.Commands (cmdClash) where

import Discord
import Discord.Types
import qualified Data.Map as M
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
cmdClash args state msg = do
    let quick = lookupFlag args "quick" "--"
        modes = lookupArgument args "m" "-"
        langs = lookupArgument args "l" "-"
    
    _ <- restCall $ R.DeleteMessage (messageChannel msg, messageId msg)

    if quick then do
            info <- liftIO $ runMaybeT $ requestGame modes langs
            sendInviteEmbed (messageChannel msg) info
    else do
        let builder = GameOptionBuilder {
            creator = Discord.Types.userId $ messageAuthor msg,
            Discord.ClashBuilder.options = GameOptions modes langs,
            mode = GameMode
        }
        msg <- sendBuilderEmbed builder $ messageChannel msg
        _ <- either (\_ -> pure ()) (\m -> liftIO $ modifyIORef state (M.insert (messageId m) builder)) msg
        pure ()


    