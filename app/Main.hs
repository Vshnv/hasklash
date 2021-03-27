{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Bot
import Discord
import Discord.Types
import System.Environment.MrEnv (envAsString)

main :: IO ()
main = do 
       putStrLn "Running..."
       botToken <- envAsString "DISCORD_BOT_TOKEN" ""
       userFacingError <- runDiscord $ def
                                           { discordToken = T.pack botToken
                                           , discordOnEvent = commandRouter }
       TIO.putStrLn userFacingError
  