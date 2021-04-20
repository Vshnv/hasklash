module Lib
    (unwrapObject
    ) where


import Data.Aeson ( Value(Object), Object )


unwrapObject :: Value -> Object
unwrapObject (Object x) = x
unwrapObject _ = error "No Object available"