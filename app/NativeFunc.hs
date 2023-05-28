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

thirdArg :: NativeFunc (Value, Position)
thirdArg = ReaderT $ \args -> return $ args !! 2

sqrtNative :: NativeFunction
sqrtNative = NativeFunction 1 $ do
    (v, pos) <- firstArg
    f <- getValueType getFloating v pos
    return (Double $ sqrt f)

getValueType :: (MonadTrans m, Monad t) => (Value -> Either InterpretErrorType a) -> Value -> Position -> m (ExceptT Exception t) a
getValueType f v pos = lift $ except $ mapLeft (throwWithOffset pos) (f v)

showNative :: NativeFunction
showNative = NativeFunction 1 $ do
    (v, pos) <- firstArg
    return (String $ show v)

clockNative :: NativeFunction
clockNative = NativeFunction 0 $ do
    time <- liftIO (getCurrentTime <&> realToFrac . utctDayTime)
    return $ Double time

vectorLengthNative :: NativeFunction
vectorLengthNative = NativeFunction 1 $ do
    (v, pos) <- firstArg
    v <- getValueType getArray v pos
    return $ Int $ V.length v

vectorMapNative :: NativeFunction
vectorMapNative = NativeFunction 2 $ do
    (v, pos) <- firstArg
    vector <- getValueType getArray v pos
    (v, pos) <- secondArg
    function <- getValueType getFunc v pos
    newVector <- V.mapM (\x -> lift $ runFunction function ([x], pos)) vector
    return $ Array newVector

vFoldrM :: Monad m => (a -> b -> m b) -> b -> V.Vector a -> m b
vFoldrM f acc vector
    | null vector = return acc
    | otherwise = do
        rest <- vFoldrM f acc (V.tail vector)
        f (V.head vector) rest

vectorFoldrNative :: NativeFunction
vectorFoldrNative = NativeFunction 3 $ do
    (v, pos) <- firstArg
    vector <- getValueType getArray v pos
    (v, pos) <- secondArg
    function <- getValueType getFunc v pos
    (v, pos) <- thirdArg
    vFoldrM (\x acc -> lift $ runFunction function ([x, acc], pos)) v vector

vectorFilterNative :: NativeFunction
vectorFilterNative = NativeFunction 2 $ do
    (v, pos) <- firstArg
    vector <- getValueType getArray v pos
    (v, pos) <- secondArg
    function <- getValueType getFunc v pos
    newVector <- V.filterM (\x -> lift $ getBool' function x pos) vector
    return $ Array newVector
    where
        getBool' :: Function -> Value -> Position -> (ExceptT Exception IO) Bool
        getBool' f x pos = do
            v <- runFunction f ([x], pos)
            except $ mapLeft (throwWithOffset pos) (getBool v)

vectorWithRangeNative :: NativeFunction
vectorWithRangeNative = NativeFunction 2 $ do
    (v, pos) <- firstArg
    lower <- getValueType getInt v pos
    (v, pos) <- secondArg
    upper <- getValueType getInt v pos
    let size = max 0 (upper - lower + 1)
    return $ Array (V.map Int $ V.fromList [lower..upper])

vectorAppendNative :: NativeFunction
vectorAppendNative = NativeFunction 2 $ do
    (v, pos) <- firstArg
    vector <- getValueType getArray v pos
    (value, pos) <- secondArg
    let newVector = V.snoc vector value
    return $ Array newVector

stringToListNative :: NativeFunction
stringToListNative = NativeFunction 1 $ do
    (v, pos) <- firstArg
    s <- getValueType getString v pos
    let vector = V.map (\x -> String [x]) (V.fromList s)
    return $ Array vector

listToStringNative :: NativeFunction
listToStringNative = NativeFunction 1 $ do
    (v, pos) <- firstArg
    list <- getValueType getArray v pos
    String . concat . V.toList <$> V.mapM (\x -> case getString x of
        Right s -> return s
        _ -> return "") list

inputNative :: NativeFunction
inputNative = NativeFunction 0 $ do
    liftIO $ hSetBuffering stdout NoBuffering
    line <- liftIO getLine
    return $ String line

putStrNative :: NativeFunction
putStrNative = NativeFunction 1 $ do
    liftIO $ hSetBuffering stdout NoBuffering
    (v, pos) <- firstArg
    s <- getValueType getString v pos
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
    ("show", showNative),
    ("map", vectorMapNative),
    ("filter", vectorFilterNative),
    ("vectorWithRange", vectorWithRangeNative),
    ("foldr", vectorFoldrNative),
    ("toList", stringToListNative),
    ("toString", listToStringNative)
    ]
