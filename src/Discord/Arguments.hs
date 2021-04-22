{-# LANGUAGE OverloadedStrings #-}
module Discord.Arguments where

import qualified Data.Text as T

lookupArgument :: [T.Text] -> T.Text -> T.Text  -> [T.Text]
lookupArgument args tag argPrefix =
    map snd $
        filter ((== (argPrefix <> tag)) . fst) $
            zip args $ tail (args ++ [""])

lookupFlag :: [T.Text] -> T.Text -> T.Text -> Bool
lookupFlag args tag argPrefix =
    (argPrefix <> tag) `elem` args




