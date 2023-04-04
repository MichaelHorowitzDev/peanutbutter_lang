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
    (v, n) <- except $ getValueType getArray v "List" pos
    return $ Int n

vectorAppendNative :: NativeFunction
vectorAppendNative = NativeFunction 2 $ \args -> do
    let (v, pos) = firstArg args
    v@(vector, size) <- except $ getValueType getArray v "List" pos
    let (value, pos) = secondArg args
    let (newVector, newSize) = append v value
    return $ Array newVector newSize
    where
        append :: (V.Vector Value, Int) -> Value -> (V.Vector Value, Int)
        append (vector, size) value
            | size < V.length vector =
                let old = (V.toList $ V.take size vector)
                    new = old ++ [value]
                in (V.fromList new, size + 1)
            | otherwise =
                let old = V.toList vector
                    new = old ++ [value]
                in (V.fromList $ new ++ new, size + 1)

firstArg :: [a] -> a
firstArg = head

secondArg :: [a] -> a
secondArg = (!! 1)

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
    ("putStr", putStrNative),
    ("append", vectorAppendNative)
    ]
