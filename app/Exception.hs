module Exception where

import Ast

data Exception = ErrMsg String
    | ReturnExcept Value
    deriving Show