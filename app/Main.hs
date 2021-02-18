{-# LANGUAGE OverloadedStrings #-}

module Main where
import System.Environment.MrEnv ( envAsString )

import Clash

main :: IO ()
main = do
  email <- envAsString "CLASH_EMAIL" ""
  password <- envAsString "CLASH_PASSWORD" ""
  resp <- login email password
  execute resp

execute :: Maybe LoginResult -> IO ()    
execute (Just result) = do
        game <- createPrivateGame [] [] result
        case game >>= Just . handleToLink of 
          {(Just a) -> putStrLn a
          ;Nothing -> return ()
          }
execute Nothing = return ()