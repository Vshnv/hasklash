{-# LANGUAGE OverloadedStrings #-}

module Bot (commandRouter) where

import Control.Monad (when,(>>=))
import Data.Text (isPrefixOf, toLower, Text, pack, unpack)
import qualified Data.Text.IO as TIO

import UnliftIO
import Discord
import Discord.Types
import qualified Discord.Requests as R
import Clash
import System.Environment.MrEnv ( envAsString )
import Data.List (find)
import Data.Maybe (fromMaybe)



commandRouter:: Event -> DiscordHandler ()
commandRouter (MessageCreate m)
              | isUserMessage m = do
                      if messageText m == "=clash" then do
                        clash <- liftIO createClash
                        let joinStr = "You have been invited to a round of Clash Of Code!\n"++
                                      " [Click here to Join!](" ++ unpack (fromMaybe "" clash) ++ ")"
                        _ <- restCall $ R.CreateMessageEmbed (messageChannel m) "" $
                                                def { createEmbedTitle = "Clash Of Code"
                                                    , createEmbedThumbnail = Just $ CreateEmbedImageUrl
                                                            "https://files.codingame.com/codingame/share_pics_clash_of_code.jpg"
                                                    , createEmbedDescription = pack joinStr
                                                    }
                        pure ()
                      else pure ()
              | otherwise = do
                 _ <- liftIO $ putStrLn "Received Bot Message!"
                 pure ()
commandRouter _ = pure ()

createClash:: IO (Maybe Text)
createClash = do
      email <- envAsString "CLASH_EMAIL" ""
      password <- envAsString "CLASH_PASSWORD" ""
      res <- login email password 
      clashHandle <- case res of
        {(Just cred) -> createPrivateGame [] [] cred
        ; Nothing -> return Nothing
        }
      return $ clashHandle >>= Just . pack . handleToLink
      
isUserMessage :: Message -> Bool
isUserMessage = not . userIsBot . messageAuthor