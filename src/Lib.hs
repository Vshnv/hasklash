module Lib
    (unwrapObject
    ) where


import Data.Aeson ( Value(Object), Object )
import qualified Data.Map as M

unwrapObject :: Value -> Object
unwrapObject (Object x) = x
unwrapObject _ = error "No Object available"


