module Interpret where

import Ast

import Data.Either.Extra
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Data.Maybe (fromJust)
import Data.IORef
import Text.Megaparsec
import InterpretError
import NativeFunc
import qualified Data.Vector as V
import Data.List (find)
import Data.List.Extra (nubOrd)

type Prog = [Stmt]

type Interpreter a = ReaderT Env (ExceptT Exception IO) a
type Store = [(Var, Val)]

maybeToMaybeT :: Applicative m => Maybe b -> MaybeT m b
maybeToMaybeT = MaybeT . pure

readStore :: Interpreter Store
readStore = do
    env <- getEnv
    liftIO $ readIORef (varEnv env)

getPrevEnv :: Interpreter (Maybe Env)
getPrevEnv = asks prevEnv

replaceEnv :: Env -> Interpreter a -> Interpreter a
replaceEnv env = local (const env)

getEnv :: Interpreter Env
getEnv = ask

modifyStore :: (Store -> Store) -> Interpreter ()
modifyStore f = do
    env <- getEnv
    liftIO $ modifyIORef (varEnv env) f

writeStore :: Store -> Interpreter ()
writeStore s = modifyStore (const s)

envLookup :: Var -> Interpreter (Maybe Val)
envLookup var = do
    env <- getEnv
    liftIO $ runMaybeT $ envLook env var
    where
        envLook :: Env -> Var -> MaybeT IO Val
        envLook (Env store prev) var = do
            s <- lift (readIORef store)
            case lookup var s of
                (Just val) -> return val
                Nothing -> maybeToMaybeT prev >>= \env -> envLook env var

newEnv :: IO Env
newEnv = do
    x <- newIORef []
    return $ Env x Nothing

envWithNative :: IO Env
envWithNative = do
    x <- newIORef (varsFromFuncs nativeFuncs)
    return $ Env x Nothing

varLookup :: Var -> Position -> Interpreter Value
varLookup var offset = do
    result <- envLookup var
    val <- lift $ except $ maybeToEither (errWithOffset offset (UnboundErr var)) result
    throwErrIf (isNull $ value val) (InterpretError (RefBeforeInit var) offset)

    scoped <- inScope var
    throwErrIf (not scoped && mutable val) (InterpretError (RefMutVar var) offset)

    return $ value val

getterLookup :: Var -> Position -> Interpreter Value
getterLookup var offset = do
    result <- lookup var <$> readStore
    lift $ except $ maybeToEither (errWithOffset offset (UnboundErr var)) (value <$> result)

inScope :: Var -> Interpreter Bool
inScope v = do
    store <- readStore
    case lookup v store of
        (Just val) -> return $ not $ isNull (value val)
        Nothing -> return False

addVar :: (Var, Value) -> Interpreter Env
addVar (var, val) = do
    let value = mutableVar val
    modifyStore ((var, value):)
    getEnv

addLet :: (Var, Value) -> Interpreter Env
addLet (var, val) = do
    let value = immutableVar val
    modifyStore ((var, value):)
    getEnv

throwErrIf :: Bool -> InterpretError -> Interpreter ()
throwErrIf True = throwErr
throwErrIf False = const $ return ()

throwErr :: InterpretError -> Interpreter a
throwErr err = lift $ throwE (InterpErr err)

varReassign :: (Var, Value) -> Position -> Interpreter Env
varReassign pair@(var, val) pos = do
    result <- envLookup var
    val <- lift $ except $ maybeToEither (errWithOffset pos (UnboundErr var)) result
    throwErrIf (not $ mutable val) (InterpretError (ReassignImmutableErr var) pos)
    throwErrIf (isNull (value val)) (InterpretError (RefBeforeInit var) pos)
    scoped <- inScope var
    throwErrIf (not scoped && mutable val) (InterpretError (ReassignMutVar var) pos)
    reassign pair
    where
        replace :: Eq a => a -> [(a, b)] -> b -> [(a, b)]
        replace key xs value = map (\x -> if key == fst x then (key, value) else x) xs
        reassign :: (Var, Value) -> Interpreter Env
        reassign (var, val) = do
            store <- readStore
            let newStore = replace var store (mutableVar val)
            writeStore newStore
            getEnv

addScope :: Interpreter Env
addScope = do
    env <- getEnv
    x <- liftIO $ newIORef []
    return $ Env x (Just env)

immutableVar :: Value -> Val
immutableVar v = Val {value=v, mutable=False}

mutableVar :: Value -> Val
mutableVar v = Val {value=v, mutable=True}

removeScope :: Env -> Maybe Env
removeScope (Env _ prev) = prev

guardEither :: Bool -> a -> Either a ()
guardEither False a = Left a
guardEither True _ = return ()

errWithOffset :: Position -> InterpretErrorType -> Exception
errWithOffset offset errType =
    let interpretError = InterpretError errType offset
    in InterpErr interpretError

binOpTypeErr :: String -> Value -> Value -> InterpretErrorType
binOpTypeErr op x y =
    let v1 = valueTypeLookup x
        v2 = valueTypeLookup y
    in BinOpTypeErr op v1 v2

unOpTypeErr :: String -> Value -> InterpretErrorType
unOpTypeErr op x =
    let v1 = valueTypeLookup x
    in UnOpTypeErr op v1

callNonFuncErr :: Value -> InterpretErrorType
callNonFuncErr = CallNonFuncErr . valueTypeLookup

subscriptNonArrayErr :: Value -> InterpretErrorType
subscriptNonArrayErr = SubscriptNonArray . valueTypeLookup

callMemberNonObject :: Value -> InterpretErrorType
callMemberNonObject = CallMemberNonObject . valueTypeLookup

nativeFuncCall :: Position -> [Exp] -> NativeFunction -> Interpreter Value
nativeFuncCall pos exps native = do
    testArity
    args <- evalArgs exps
    lift $ runNativeFunc native args
    where
        testArity = (lift . except) $ guardEither (length exps == funcArity native)
            (errWithOffset pos (ArityErr (funcArity native) (length exps)))
        evalArgs :: [Exp] -> Interpreter [(Value, Position)]
        evalArgs [] = return []
        evalArgs (x:xs) = do
            let pos = getExpPosition x
            val <- eval x
            vals <- evalArgs xs
            return $ (val, pos) : vals

instanceCall :: ([String], [Stmt], Env) -> [Value] -> Position -> Interpreter Value
instanceCall (params, stmts, env) args pos = do
    testArity params args
    let vars = zip params args
    env <- replaceEnv env (addConstStoreScope vars)
    env <- replaceEnv env (execStmts stmts)
    return $ ClassInstance env
    where
        testArity :: [String] -> [Value] -> Interpreter ()
        testArity xs ys = (lift . except) $ guardEither (params == args)
            (errWithOffset pos (ArityErr params args))
            where
                params = length xs
                args = length ys
        addConstStoreScope :: [(Var, Value)] -> Interpreter Env
        addConstStoreScope store = do
            env <- getEnv
            let consts = map (\(x, y) -> (x, immutableVar y)) store
            vars <- liftIO (newIORef consts)
            return $ Env vars (Just env)

funcCall :: Function -> [Value] -> Position -> Interpreter Value
funcCall function args pos = do
    let (Function params stmts funcEnv) = function
    testArity params args
    let vars = zip params args
    env <- replaceEnv funcEnv (addConstStoreScope vars)
    lift $ ExceptT $ runExceptT (runReaderT (execStmts stmts) env) >>= \result -> case result of
        Right env -> return $ Right Void
        Left (ReturnExcept env expStmt pos) -> runExceptT $ runReaderT (eval expStmt) env
        Left a -> return $ Left a
    where
        testArity :: [String] -> [Value] -> Interpreter ()
        testArity xs ys = (lift . except) $ guardEither (params == args)
            (errWithOffset pos (ArityErr params args))
            where
                params = length xs
                args = length ys
        addConstStoreScope :: [(Var, Value)] -> Interpreter Env
        addConstStoreScope store = do
            env <- getEnv
            let consts = map (\(x, y) -> (x, immutableVar y)) store
            vars <- liftIO (newIORef consts)
            return $ Env vars (Just env)

add :: Value -> Value -> Position -> Either Exception Value
add x y pos = case (x, y) of
    (Int x, Int y) -> return $ Int (x + y)
    (Float x, Float y) -> return $ Float (x + y)
    _ -> Left $ errWithOffset pos (binOpTypeErr "add" x y)

sub :: Value -> Value -> Position -> Either Exception Value
sub x y pos = case (x, y) of
    (Int x, Int y) -> return $ Int (x - y)
    (Float x, Float y) -> return $ Float (x - y)
    _ -> Left $ errWithOffset pos (binOpTypeErr "subtract" x y)

mul :: Value -> Value -> Position -> Either Exception Value
mul (Int x) (Int y) pos = Right $ Int (x * y)
mul (Float x) (Float y) pos = Right $ Float (x * y)
mul x y pos = Left $ errWithOffset pos (binOpTypeErr "multiply" x y)

divide :: Value -> Value -> Position -> Either Exception Value
divide x y pos = case (x, y) of
    (Int x, Int y) -> return $ Int (x `div` y)
    (Float x, Float y) -> return $ Float (x / y)
    _ -> Left $ errWithOffset pos (binOpTypeErr "divide" x y)

greater :: Value -> Value -> Position -> Either Exception Value
greater (Int x) (Int y) pos = return $ Bool $ x > y
greater (Float x) (Float y) pos = return $ Bool $ x > y
greater x y pos = Left $ errWithOffset pos (binOpTypeErr "compare" x y)

greaterEqual :: Value -> Value -> Position -> Either Exception Value
greaterEqual x y pos = case (x, y) of
    (Int x, Int y) -> return $ Bool $ x >= y
    (Float x, Float y) -> return $ Bool $ x >= y
    _ -> Left $ errWithOffset pos (binOpTypeErr "compare" x y)

lesser :: Value -> Value -> Position -> Either Exception Value
lesser x y pos = case (x, y) of
    (Int x, Int y) -> return $ Bool $ x < y
    (Float x, Float y) -> return $ Bool $ x < y
    _ -> Left $ errWithOffset pos (binOpTypeErr "compare" x y)

lesserEqual :: Value -> Value -> Position -> Either Exception Value
lesserEqual x y pos = case (x, y) of
    (Int x, Int y) -> return $ Bool $ x <= y
    (Float x, Float y) -> return $ Bool $ x <= y
    _ -> Left $ errWithOffset pos (binOpTypeErr "compare" x y)

equal :: Value -> Value -> Position -> Either Exception Value
equal x y pos = case (x, y) of
    (Int x, Int y) -> return $ Bool $ x == y
    (Float x, Float y) -> return $ Bool $ x == y
    (String x, String y) -> return $ Bool $ x == y
    (Bool x, Bool y) -> return $ Bool $ x == y
    _ -> Left $ errWithOffset pos (binOpTypeErr "compare" x y)

notEqual :: Value -> Value -> Position -> Either Exception Value
notEqual x y pos = case (x, y) of
    (Int x, Int y) -> return $ Bool $ x /= y
    (Float x, Float y) -> return $ Bool $ x /= y
    (String x, String y) -> return $ Bool $ x /= y
    (Bool x, Bool y) -> return $ Bool $ x /= y
    _ -> Left $ errWithOffset pos (binOpTypeErr "compare" x y)

negateVal :: Value -> Position -> Either Exception Value
negateVal x pos = case x of
    (Int x) -> return $ Int (negate x)
    (Float x) -> return $ Float (negate x)
    _ -> Left $ errWithOffset pos (unOpTypeErr "negate" x)

bang :: Value -> Position -> Either Exception Value
bang x pos = case x of
    (Bool x) -> return $ Bool (not x)
    _ -> Left $ errWithOffset pos (unOpTypeErr "negate" x)

printVal :: Value -> IO ()
printVal (String s) = putStrLn s
printVal (Float f) = print f
printVal (Int n) = print n
printVal (Bool b) = putStrLn (if b then "true" else "false")
printVal (Func {}) = putStrLn "<func>"
printVal (NativeFunc {}) = putStrLn "<native_fn>"
printVal (Array v) = print v
printVal Void = putStrLn "Void"
printVal Null = putStrLn "Null"
printVal (Class {}) = putStrLn "<class>"
printVal (ClassInstance {}) = putStrLn "<object>"
printVal v = putStrLn $ "cannot print value: " ++ show v

eval :: Exp -> Interpreter Value
eval (Lit n pos) = return n
eval (Var s pos) = varLookup s pos
eval (Add x y pos) = do
    v1 <- eval x
    v2 <- eval y
    lift $ except $ add v1 v2 pos
eval (Sub x y pos) = do
    v1 <- eval x
    v2 <- eval y
    lift $ except $ sub v1 v2 pos
eval (Mul x y pos) = do
    v1 <- eval x
    v2 <- eval y
    lift $ except $ mul v1 v2 pos
eval (Div x y pos) = do
    v1 <- eval x
    v2 <- eval y
    lift $ except $ divide v1 v2 pos
eval (Greater x y pos) = do
    v1 <- eval x
    v2 <- eval y
    lift $ except $ greater v1 v2 pos
eval (Less x y pos) = do
    v1 <- eval x
    v2 <- eval y
    lift $ except $ lesser v1 v2 pos
eval (GreaterEqual x y pos) = do
    v1 <- eval x
    v2 <- eval y
    lift $ except $ greaterEqual v1 v2 pos
eval (LessEqual x y pos) = do
    v1 <- eval x
    v2 <- eval y
    lift $ except $ lesserEqual v1 v2 pos
eval (Equal x y pos) = do
    v1 <- eval x
    v2 <- eval y
    lift $ except $ equal v1 v2 pos
eval (NotEqual x y pos) = do
    v1 <- eval x
    v2 <- eval y
    lift $ except $ notEqual v1 v2 pos
eval (Negate x pos) = do
    v1 <- eval x
    lift $ except $ negateVal v1 pos
eval (Bang x pos) = do
    v1 <- eval x
    lift $ except $ bang v1 pos
eval (CallFunc exp args pos) = do
    value <- eval exp
    case value of
        (Func params stmts funcEnv) -> do
            args <- evalArgs args
            funcCall (Function params stmts funcEnv) args pos
        (NativeFunc native) -> do
            nativeFuncCall pos args native
        (Class params stmts env) -> do
            args <- evalArgs args
            instanceCall (params, stmts, env) args pos
        _ -> throwErr (InterpretError (callNonFuncErr value) pos)
    where
        evalArgs :: [Exp] -> Interpreter [Value]
        evalArgs [] = return []
        evalArgs (x:xs) = do
            val <- eval x
            vals <- evalArgs xs
            return (val:vals)
eval (Lambda params exp pos) = do
    env <- getEnv
    lift $ except $ guardEither (nubOrd params == params) (errWithOffset pos DuplicateFuncArgs)
    let function = Func params [ReturnStmt exp (Position 0 0)] env
    return function
eval (ArrayDef exps pos) = do
    vals <- evalExps exps
    return $ Array (V.fromList vals)
    where
        evalExps :: [Exp] -> Interpreter [Value]
        evalExps [] = return []
        evalExps (x:xs) = do
            val <- eval x
            vals <- evalExps xs
            return (val:vals)
eval (Subscript exp sub pos) = do
    value <- eval exp
    vector <- lift $ except $ maybeToEither (errWithOffset pos $ subscriptNonArrayErr value) (getArray value)
    index <- eval sub >>= \index -> lift $ except $ maybeToEither (errWithOffset pos $ WrongTypeErr (valueTypeLookup index) "Int") (getInt index)
    lift $ except $ guardEither (V.length vector > index && index >= 0) (errWithOffset pos $ IndexOutOfBounds index (V.length vector))
    return $ vector V.! index
eval (Getter exp var pos) = do
    value <- eval exp
    case value of
        ClassInstance env -> replaceEnv env $ getterLookup var pos
        _ -> throwErr (InterpretError (callMemberNonObject value) pos)

initVars :: [Stmt] -> [Stmt]
initVars stmts = getDecs stmts ++ stmts
    where
        getDecs :: [Stmt] -> [Stmt]
        getDecs [] = []
        getDecs (x:xs) = case x of
            (VarAssign s _ _) -> VarAssign s (Lit Null (Position 0 0)) (Position 0 0) : getDecs xs
            (LetAssign s _ _) -> LetAssign s (Lit Null (Position 0 0)) (Position 0 0) : getDecs xs
            _ -> getDecs xs

exec :: Stmt -> Interpreter Env
exec (VarAssign s exp pos) = do
    scoped <- inScope s
    value <- eval exp
    if scoped then do
        throwErrIf (not $ isNull value) (InterpretError (InvalidRedeclarationOfVar s) pos)
        getEnv
    else addVar (s, value)
exec (LetAssign s exp pos) = do
    scoped <- inScope s
    value <- eval exp
    if scoped then do
        throwErrIf (not $ isNull value) (InterpretError (InvalidRedeclarationOfVar s) pos)
        getEnv
    else addLet (s, value)
exec (VarReassign s exp pos) = do
    value <- eval exp
    varReassign (s, value) pos
exec (While exp stmts pos) = do
    value <- eval exp
    case value of
        (Bool True) -> do
            env' <- execStmts stmts
            local (const env') (execStmts [While exp stmts pos])
        (Bool False) -> getEnv
        v -> throwErr $ InterpretError (WrongTypeErr (valueTypeLookup v) "Bool") (getExpPosition exp)
exec (If [] stmt pos) = execStmts stmt
exec (If ((exp, stmt):xs) stmt' pos) = do
    value <- eval exp
    case value of
        (Bool True) -> execStmts stmt
        (Bool False) -> exec (If xs stmt' pos)
        v -> throwErr $ InterpretError (WrongTypeErr (valueTypeLookup v) "Bool") (getExpPosition exp)
exec (FuncDef s args stmt pos) = do
    env <- getEnv
    scoped <- inScope s
    lift $ except $ guardEither (not scoped) (errWithOffset pos (InvalidRedeclarationOfVar s))
    lift $ except $ guardEither (nubOrd args == args) (errWithOffset pos DuplicateFuncArgs)
    let varInits = initVars stmt
    let function = Func args varInits env
    addLet (s, function)
exec (ClassDef s args stmts pos) = do
    scoped <- inScope s
    lift $ except $ guardEither (not scoped) (errWithOffset pos (InvalidRedeclarationOfVar s))
    case find (not . validClassStmt) stmts of
        Just stmt -> do
            throwErr $ InterpretError (InvalidClassStmt $ getStmtName stmt) (getStmtPosition stmt)
        Nothing -> return ()
    env <- getEnv
    let classDec = Class args stmts env
    addLet (s, classDec)
exec (CallExp exp pos) = case exp of
    (CallFunc name args pos) -> eval exp >> getEnv
    _ -> eval exp >> getEnv
exec (ReturnStmt expStmt pos) = do
    env <- getEnv
    lift $ throwE $ ReturnExcept env expStmt pos
exec (Print exp pos) = do
    val <- eval exp
    liftIO (printVal val)
    getEnv

execStmts :: [Stmt] -> Interpreter Env
execStmts [] = getEnv
execStmts (x:xs) = do
    env <- exec x
    local (const env) (execStmts xs)

validClassStmt :: Stmt -> Bool
validClassStmt stmt = case stmt of
    LetAssign {} -> True
    VarAssign {} -> True
    FuncDef {} -> True
    ClassDef {} -> True
    _ -> False

performTransformations :: [Stmt] -> [Stmt]
performTransformations = initVars

runProgram :: Env -> Prog -> ExceptT InterpretError IO ()
runProgram env stmt = ExceptT $ do
    let transformed = performTransformations stmt
    result <- runExceptT $ runReaderT (execStmts transformed) env
    case result of
        Right _ -> return $ return ()
        Left (InterpErr err) -> return $ Left err
        Left (ReturnExcept env expStmt pos) -> return $ Left (InterpretError ReturnNotInFunction pos)

execNew :: Prog -> ExceptT InterpretError IO ()
execNew stmt = liftIO envWithNative >>= \env -> runProgram env stmt
