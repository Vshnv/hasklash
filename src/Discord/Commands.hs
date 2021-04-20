module Discord.Commands () where

import Discord
import Discord.Types
import qualified Discord.Requests as R

cmdClash :: Bool -> IO ()
cmdClash quick = print quick