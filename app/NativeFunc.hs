module NativeFunc (
    NativeFunction,
    nativeFuncs,
    varsFromFuncs
) where

import Data.Time.Clock
import Data.Functor
import Ast
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import InterpretError
import Data.Either.Extra

guardEither :: Bool -> a -> Either a ()
guardEither False a = Left a
guardEither True _ = return ()

getFloat :: Value -> Maybe Float
getFloat (Float f) = Just f
getFloat _ = Nothing

throwWithOffset :: Position -> InterpretErrorType -> Exception
throwWithOffset offset errType =
    let interpretError = InterpretError errType offset
    in InterpErr interpretError

type NativeFunction = [(Value, Position)] -> ExceptT Exception IO Value

sqrtNative :: NativeFunction
sqrtNative args = do
    let (v, pos) = head args
    f <- except $ maybeToEither (throwWithOffset pos $ WrongTypeErr (valueTypeLookup v) "Float") (getFloat v)
    return (Float $ sqrt f)

clockNative :: NativeFunction
clockNative _ = do
    time <- lift (getCurrentTime <&> realToFrac . utctDayTime)
    return $ Float time

varFromFunc :: String -> Int -> NativeFunction -> (Var, Val)
varFromFunc s arr f = (s, Val { value = NativeFunc arr f, mutable = False })

varsFromFuncs :: [(String, Int, NativeFunction)] -> [(Var, Val)]
varsFromFuncs [] = []
varsFromFuncs ((s, arr, f):xs) = varFromFunc s arr f : varsFromFuncs xs

nativeFuncs :: [(String, Int, NativeFunction)]
nativeFuncs = [
    ("sqrt", 1, sqrtNative),
    ("clock", 0, clockNative)
    ]