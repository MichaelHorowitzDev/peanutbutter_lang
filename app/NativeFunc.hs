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
import System.IO
import qualified Data.Vector as V

guardEither :: Bool -> a -> Either a ()
guardEither False a = Left a
guardEither True _ = return ()

throwWithOffset :: Position -> InterpretErrorType -> Exception
throwWithOffset offset errType =
    let interpretError = InterpretError errType offset
    in InterpErr interpretError

sqrtNative :: NativeFunction
sqrtNative = NativeFunction 1 $ \args -> do
    let (v, pos) = head args
    f <- except $ getValueType getFloat v "Float" pos
    return (Float $ sqrt f)

clockNative :: NativeFunction
clockNative = NativeFunction 0 $ \_ -> do
    time <- lift (getCurrentTime <&> realToFrac . utctDayTime)
    return $ Float time

vectorLengthNative :: NativeFunction
vectorLengthNative = NativeFunction 1 $ \args -> do
    let (v, pos) = head args
    v <- except $ getValueType getArray v "List" pos
    return $ Int (V.length v)

getValueType :: (Value -> Maybe a) -> Value -> String -> Position -> Either Exception a
getValueType f v expected pos = maybeToEither (throwWithOffset pos $ WrongTypeErr (valueTypeLookup v) expected) (f v)

inputNative :: NativeFunction
inputNative = NativeFunction 0 $ \_ -> do
    lift $ hSetBuffering stdout NoBuffering
    line <- lift getLine
    return $ String line

putStrNative :: NativeFunction
putStrNative = NativeFunction 1 $ \args -> do
    lift $ hSetBuffering stdout NoBuffering
    let (v, pos) = head args
    s <- except $ getValueType getString v "String" pos
    lift (putStr s)
    return Void

varFromFunc :: String -> NativeFunction -> (Var, Val)
varFromFunc s n = (s, Val { value = NativeFunc n, mutable = False })

varsFromFuncs :: [(String, NativeFunction)] -> [(Var, Val)]
varsFromFuncs [] = []
varsFromFuncs ((s, n):xs) = varFromFunc s n : varsFromFuncs xs

nativeFuncs :: [(String, NativeFunction)]
nativeFuncs = [
    ("sqrt", sqrtNative),
    ("clock", clockNative),
    ("length", vectorLengthNative),
    ("input", inputNative),
    ("putStr", putStrNative)
    ]
