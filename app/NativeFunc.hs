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
import Control.Monad.Reader

guardEither :: Bool -> a -> Either a ()
guardEither False a = Left a
guardEither True _ = return ()

throwWithOffset :: Position -> InterpretErrorType -> Exception
throwWithOffset offset errType =
    let interpretError = InterpretError errType offset
    in InterpErr interpretError

type NativeFunc a = ReaderT [(Value, Position)] (ExceptT Exception IO) a

firstArg :: NativeFunc (Value, Position)
firstArg = ReaderT $ \args -> return $ head args

secondArg :: NativeFunc (Value, Position)
secondArg = ReaderT $ \args -> return $ args !! 1

sqrtNative :: NativeFunction
sqrtNative = NativeFunction 1 $ do
    (v, pos) <- firstArg
    f <- getValueType getFloating v "Float" pos
    return (Float $ sqrt f)

getValueType :: (MonadTrans m, Monad t) => (Value -> Maybe a) -> Value -> String -> Position -> m (ExceptT Exception t) a
getValueType f v expected pos =
    lift $ except $ maybeToEither (throwWithOffset pos $ WrongTypeErr (valueTypeLookup v) expected) (f v)

showNative :: NativeFunction
showNative = NativeFunction 1 $ do
    (v, pos) <- firstArg
    return (String $ show v)

clockNative :: NativeFunction
clockNative = NativeFunction 0 $ do
    time <- liftIO (getCurrentTime <&> realToFrac . utctDayTime)
    return $ Float time

vectorLengthNative :: NativeFunction
vectorLengthNative = NativeFunction 1 $ do
    (v, pos) <- firstArg
    (v, n) <- getValueType getArray v "List" pos
    return $ Int n

vectorAppendNative :: NativeFunction
vectorAppendNative = NativeFunction 2 $ do
    (v, pos) <- firstArg
    v@(vector, size) <- getValueType getArray v "List" pos
    (value, pos) <- secondArg
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

inputNative :: NativeFunction
inputNative = NativeFunction 0 $ do
    liftIO $ hSetBuffering stdout NoBuffering
    line <- liftIO getLine
    return $ String line

putStrNative :: NativeFunction
putStrNative = NativeFunction 1 $ do
    liftIO $ hSetBuffering stdout NoBuffering
    (v, pos) <- firstArg
    s <- getValueType getString v "String" pos
    liftIO (putStr s)
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
    ("append", vectorAppendNative),
    ("show", showNative)
    ]
