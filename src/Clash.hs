{-# LANGUAGE OverloadedStrings #-}

module Clash where

import Network.HTTP.Req


defaultHeaders :: Option scheme
defaultHeaders =
    header "accept"  "application/json, text/plain, */*" <>
    header "accept-language" "[en-US,en;q=0.9]" <>
    header "sec-ch-ua" "\"Google Chrome\";v=\"87\", \" Not;A Brand\";v=\"99\", \"Chromium\";v=\"87\"" <>
    header "sec-fetch-dest" "empty" <>
    header "sec-fetch-mode" "cors" <>
    header "sec-fetch-site" "same-origin" <>
    header "refferer" "https://www.codingame.com/multiplayer/clashofcode" <>
    header "reffererPolicy" "strict-origin-when-cross-origin"
