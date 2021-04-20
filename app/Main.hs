module Main where

import Lib
import Clash.Auth
import Clash.Game
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Functor

main :: IO (Maybe ())
main =  runMaybeT $ do
    token <- createSession (Credentials "String" "String") :: MaybeT IO SessionToken
    liftIO $ print token