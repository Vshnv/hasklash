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
import Discord.Commands
import Discord.Router
import qualified Data.Map as M

main :: IO ()
main =  do
    putStrLn "Running..."
    botToken <- getEnv "DISCORD_BOT_TOKEN"
    let prefix = '='
    let routes = M.fromList [("clash", cmdClash)]
    userFacingError <- runDiscord $ def
                                        {discordToken = T.pack botToken
                                        ,discordOnEvent = commandRouter prefix routes }
    TIO.putStrLn userFacingError