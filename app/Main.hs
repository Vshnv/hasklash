{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Clash.Auth
import Clash.Game
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Functor
import System.Environment
import Data.Maybe
import Discord
import Discord.Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord.Commands ( cmdClash )
import Discord.Router ( commandRouter, eventRouter, reactionRemovalRouter, reactionAddRouter )
import qualified Data.Map as M
import Data.Map
import Data.IORef
import Discord.ClashBuilder
import Discord.Reactions

builderMap :: IO (IORef (Map MessageId GameOptionBuilder))
builderMap = newIORef empty 


main :: IO ()
main =  do
    putStrLn "Running..."
    botToken <- getEnv "DISCORD_BOT_TOKEN"
    builderRef <- Main.builderMap
    userFacingError <- runDiscord $ def
                                        {discordToken = T.pack botToken
                                        ,discordOnEvent = eventRouter eventListeners builderRef}
    TIO.putStrLn userFacingError

prefix = '='
commandRoutes = M.fromList [("clash", cmdClash)]

reactionAddRoutes = 
        [
            trackGameReactionAdd,
            trackGameSubmit
        ]

reactionRemoveRoutes = 
        [
            trackGameReactionRemove
        ]

eventListeners builderRef = 
        [
            commandRouter prefix commandRoutes builderRef, 
            reactionAddRouter reactionAddRoutes builderRef,
            reactionRemovalRouter reactionRemoveRoutes builderRef
        ]
