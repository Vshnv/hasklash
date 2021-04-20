module Discord.Arguments where

import qualified Data.Text as T

lookupArgument :: [T.Text] -> T.Text -> T.Text  -> [T.Text]
lookupArgument args tag argPrefix =
    map snd $
        filter ((== (argPrefix <> tag)) . fst) $
            zip (map (argPrefix <>) args) $ tail args

lookupFlag :: [T.Text] -> T.Text -> T.Text -> Bool
lookupFlag args tag argPrefix =
    any ((== (argPrefix <> tag)) . fst) $
            zip (map (argPrefix <>) args) $ tail args
