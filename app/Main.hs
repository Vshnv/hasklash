module Main where

import Lib
import Clash

main :: IO ()
main =  createSession (Credentials "String" "String") >>= print
