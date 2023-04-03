module InterpretError where

import Text.Megaparsec
import Position

data InterpretErrorType = InvalidRedeclarationOfVar String
    | RefBeforeInit String
    | RefMutVar String
    | ReassignMutVar String
    | UnboundErr String
    | ReassignImmutableErr String
    | BinOpTypeErr String String String
    | UnOpTypeErr String String
    | WrongTypeErr String String
    | CallNonFuncErr String
    | SubscriptNonArray String
    | IndexOutOfBounds Int Int
    | ArityErr Int Int
    | InvalidClassStmt String
    | CallMemberNonObject String
    | DuplicateFuncArgs
    | ReturnNotInFunction
    deriving (Eq, Ord, Show)

data InterpretError = InterpretError { errType :: InterpretErrorType, offset :: Position }
    deriving (Eq, Ord, Show)

instance ShowErrorComponent InterpretError where
    showErrorComponent a = case errType a of
        (InvalidRedeclarationOfVar var) -> "invalid redeclaration of `" ++ var ++ "`"
        (RefBeforeInit var) -> "attempt to reference variable `" ++ var ++ "` before it was initialized"
        (RefMutVar var) -> "cannot reference mutable variable `" ++ var ++ "` declared in outer scope"
        (ReassignMutVar var) -> "cannot reassign mutable variable `" ++ var ++ "` declared in outer scope"
        (UnboundErr var) -> "unbound variable `" ++ var ++ "`"
        (BinOpTypeErr op v1 v2) -> "cannot " ++ op ++ " values of type `" ++ v1 ++ "` and type `" ++ v2 ++ "`"
        (UnOpTypeErr op v1) -> "cannot " ++ op ++ " value of type `" ++ v1 ++ "`"
        (ReassignImmutableErr var) -> "cannot reassign immutable variable `" ++ var ++ "`"
        (WrongTypeErr x y) -> "cannot use value of type `" ++ x ++ "` where value of type `" ++ y ++ "` was expected"
        (CallNonFuncErr x) -> "cannot call value of non function type `" ++ x ++ "`"
        (SubscriptNonArray x) -> "cannot subscript value of non array type `" ++ x ++ "`"
        (IndexOutOfBounds x y) -> "attempt to index list but index `" ++ show x ++ "` is outside bounds range of (0, " ++ show (y - 1) ++ ")"
        (ArityErr x y) -> "incorrect number of arguments passed to function" ++
            "\n" ++ show x ++ " parameters expected but " ++ show y ++ " arguments passed in"
        (InvalidClassStmt s) -> "invalid `" ++ s ++ "` statement found in class declaration \n"
        (CallMemberNonObject v1) -> "attempt to call member on non object type `" ++ v1 ++ "`"
        DuplicateFuncArgs -> "duplicate function arguments found"
        ReturnNotInFunction -> "return statement not nested in function"
    errorComponentLen a = posLength $ offset a
