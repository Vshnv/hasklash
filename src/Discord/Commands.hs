module Discord.Commands () where

import Discord
import Discord.Types 
import Data.Text as T
import qualified Discord.Requests as R
import Discord.Arguments

cmdClash :: [Text] -> Message -> DiscordHandler ()
cmdClash args msg =
    let quick = lookupFlag args "quick" "--"
        modes = lookupArgument args "m" "-"
        langs = lookupArgument args "l" "-"
    in 
        if quick then createGame [] []
        else  