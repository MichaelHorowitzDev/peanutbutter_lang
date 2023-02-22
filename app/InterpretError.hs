module InterpretError where

import Text.Megaparsec
import Position

data InterpretErrorType = InvalidRedeclarationOfVar String
    | RefBeforeInit String
    | RefMutVar String
    | UnboundErr String
    | ReassignImmutableErr String
    | BinOpTypeErr String String String
    | UnOpTypeErr String String
    | WrongTypeErr String String
    | CallNonFuncErr String
    | ArityErr Int Int
    deriving (Eq, Ord, Show)

data InterpretError = InterpretError { errType :: InterpretErrorType, offset :: Position }
    deriving (Eq, Ord, Show)

instance ShowErrorComponent InterpretError where
    showErrorComponent a = case errType a of
        (InvalidRedeclarationOfVar var) -> "invalid redeclaration of `" ++ var ++ "`"
        (RefBeforeInit var) -> "attempt to reference variable `" ++ var ++ "` before it was initialized"
        (RefMutVar var) -> "cannot reference mutable variable `" ++ var ++ "` declared in outer scope"
        (UnboundErr var) -> "unbound variable `" ++ var ++ "`"
        (BinOpTypeErr op v1 v2) -> "cannot " ++ op ++ " values of type `" ++ v1 ++ "` and type `" ++ v2 ++ "`"
        (UnOpTypeErr op v1) -> "cannot " ++ op ++ " value of type `" ++ v1 ++ "`"
        (ReassignImmutableErr var) -> "cannot reassign immutable variable `" ++ var ++ "`"
        (WrongTypeErr x y) -> "cannot use value of type `" ++ x ++ "` where value of type `" ++ y ++ "` was expected"
        (CallNonFuncErr x) -> "cannot call value of non function type `" ++ x ++ "`"
        (ArityErr x y) -> "incorrect number of arguments passed to function" ++
            "\n" ++ show x ++ " parameters expected but " ++ show y ++ " arguments passed in"
    errorComponentLen a = posLength $ offset a
