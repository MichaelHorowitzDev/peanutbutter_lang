module Exception where

import Ast

data Exception = ErrMsg String
    | Return Value
    deriving Show