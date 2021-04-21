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


cmdClash :: [Text] -> Message -> DiscordHandler ()
cmdClash args msg =
    let quick = lookupFlag args "quick" "--"
        modes = lookupArgument args "m" "-"
        langs = lookupArgument args "l" "-"
    in
        if quick then do
            info <- liftIO $ runMaybeT $ requestPrompt modes langs
            let joinStr =   "You have been invited to a round of Clash Of Code!\n" ++
                            " [Click here to Join!](" ++ unpack (maybe ") : Error Fetching Link...(" (handleLink . handle) info) ++ ")"
            _ <- restCall $ R.CreateMessageEmbed (messageChannel msg) "" $
                    def {   createEmbedTitle = "Clash Of Code"
                        ,   createEmbedThumbnail = Just $ CreateEmbedImageUrl
                            "https://files.codingame.com/codingame/share_pics_clash_of_code.jpg"
                        ,   createEmbedDescription = pack joinStr
                        }
            pure ()
        else do
            liftIO $ print args
            info <- liftIO $ runMaybeT $ requestPrompt (modeRequest modes) (langRequest langs)
            pure ()

requestPrompt :: [Text] -> [Text] -> MaybeT IO GameInfo
requestPrompt modes langs = do
    email <- liftIO envEmail
    password <- liftIO envPassword
    session <- createSession $ Credentials { email = T.pack email , password = T.pack password }
    createGame GameOptions { modes = modes, languages = langs } session

modeRequest = id

langRequest = id