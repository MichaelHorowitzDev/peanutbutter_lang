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
import qualified Data.Map.Strict as Map
import Data.Functor ((<&>))

type Prog = [Stmt]

type Interpreter a = ReaderT Env (ExceptT Exception IO) a
type Store = Map.Map Var Val

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
            case Map.lookup var s of
                (Just val) -> return val
                Nothing -> maybeToMaybeT prev >>= \env -> envLook env var

newEnv :: IO Env
newEnv = do
    x <- newIORef Map.empty
    return $ Env x Nothing

envWithNative :: IO Env
envWithNative = do
    x <- newIORef (Map.fromList $ varsFromFuncs nativeFuncs)
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
    result <- Map.lookup var <$> readStore
    lift $ except $ maybeToEither (errWithOffset offset (UnboundErr var)) (value <$> result)

inScope :: Var -> Interpreter Bool
inScope v = do
    store <- readStore
    case Map.lookup v store of
        (Just val) -> return $ not $ isNull (value val)
        Nothing -> return False

addVal :: (Var, Val) -> Interpreter Env
addVal (var, val) = do
    modifyStore (Map.insert var val)
    getEnv

addVar :: (Var, Value) -> Interpreter Env
addVar (var, value) = addVal (var, mutableVar value)

addLet :: (Var, Value) -> Interpreter Env
addLet (var, value) = addVal (var, immutableVar value)

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
        reassign :: (Var, Value) -> Interpreter Env
        reassign (var, val) = do
            store <- readStore
            let newStore = Map.insert var (mutableVar val) store
            writeStore newStore
            getEnv

addScope :: Interpreter Env
addScope = do
    env <- getEnv
    x <- liftIO $ newIORef Map.empty
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
    lift $ runReaderT (runNativeFunc native) args
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
    return $ DataInstance env
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
            let consts = Map.fromList $ map (\(x, y) -> (x, immutableVar y)) store
            vars <- liftIO (newIORef consts)
            return $ Env vars (Just env)

copyVarEnv :: Env -> IO Env
copyVarEnv env = do
    store <- readIORef $ varEnv env
    store' <- newIORef store
    return $ Env store' (prevEnv env)

bindSelf :: Env -> Function -> Value -> IO Function
bindSelf env (Function params stmts _ _) value = do
    env <- copyVarEnv env
    let val = immutableVar value
    modifyIORef (varEnv env) (Map.insert "self" val)
    let function = createRunFunction params stmts env
    return function

createRunFunction :: [String] -> [Stmt] -> Env -> Function
createRunFunction params stmts funcEnv = Function params stmts funcEnv $ \(args, pos) -> do
    except $ testArity params args pos
    let vars = zip params args
    env <- liftIO (addConstStoreScope funcEnv vars)
    runReaderT makeCall env
    where
        testArity :: [String] -> [Value] -> Position -> Either Exception ()
        testArity xs ys pos = guardEither (params == args)
            (errWithOffset pos (ArityErr params args))
            where
                params = length xs
                args = length ys
        addConstStoreScope :: Env -> [(Var, Value)] -> IO Env
        addConstStoreScope env store = do
            let consts = Map.fromList $ map (\(x, y) -> (x, immutableVar y)) store
            vars <- newIORef consts
            return $ Env vars (Just env)
        makeCall :: Interpreter Value
        makeCall = do
            env <- ask
            lift $ ExceptT $ runExceptT (runReaderT (execStmts stmts) env) >>= \result -> case result of
                Right env -> return $ Right Void
                Left (ReturnExcept env expStmt pos) -> runExceptT $ runReaderT (eval expStmt) env
                Left a -> return $ Left a

funcCall :: Function -> [Value] -> Position -> Interpreter Value
funcCall function args pos = lift $ (runFunction function) (args, pos)

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

and' :: Value -> Value -> Position -> Either Exception Value
and' x y pos = case (x, y) of
    (Bool x, Bool y) -> return $ Bool $ x && y
    _ -> Left $ errWithOffset pos (binOpTypeErr "and" x y)

or' :: Value -> Value -> Position -> Either Exception Value
or' x y pos = case (x, y) of
    (Bool x, Bool y) -> return $ Bool $ x || y
    _ -> Left $ errWithOffset pos (binOpTypeErr "or" x y)

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
printVal (Data {}) = putStrLn "<data>"
printVal (DataInstance {}) = putStrLn "<data_instance>"
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
eval (And x y pos) = do
    v1 <- eval x
    v2 <- eval y
    lift $ except $ and' v1 v2 pos
eval (Or x y pos) = do
    v1 <- eval x
    v2 <- eval y
    lift $ except $ or' v1 v2 pos
eval (Negate x pos) = do
    v1 <- eval x
    lift $ except $ negateVal v1 pos
eval (Bang x pos) = do
    v1 <- eval x
    lift $ except $ bang v1 pos
eval (CallFunc exp args pos) = do
    value <- eval exp
    case value of
        (Func function) -> do
            args <- evalArgs args
            funcCall function args pos
        (NativeFunc native) -> do
            nativeFuncCall pos args native
        (Data params stmts env) -> do
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
    lift $ except $ guardEither (nubOrd params == params) (errWithOffset pos DuplicateFuncParams)
    let function = Func $ createRunFunction params [ReturnStmt exp (Position 0 0)] env
    return function
eval (ArrayDef exps pos) = do
    vals <- evalExps exps
    let vector = V.fromList vals
    return $ Array vector
    where
        evalExps :: [Exp] -> Interpreter [Value]
        evalExps [] = return []
        evalExps (x:xs) = do
            val <- eval x
            vals <- evalExps xs
            return (val:vals)
eval (Subscript exp sub pos) = do
    value <- eval exp
    case value of
        (Array vector) -> do
            index <- getIndex (V.length vector)
            return $ vector V.! index
        _ -> throwErr $ InterpretError (SubscriptNonArray (valueTypeLookup value)) pos
    where
        getIndex :: Int -> Interpreter Int
        getIndex n = do
            index <- eval sub >>= \index -> lift $ except $ maybeToEither (errWithOffset pos $ WrongTypeErr (valueTypeLookup index) "Int") (getInt index)
            lift $ except $ guardEither (n > index && index >= 0) (errWithOffset pos $ IndexOutOfBounds index n)
            return index
eval (Slice exp start stop pos) = do
    value <- eval exp
    case value of
        (Array vector) -> do
            start <- getLower start
                <&> (\x -> if x < 0 then V.length vector + x else x)
                <&> min (V.length vector)
                <&> max 0
            end <- getHigher stop
                <&> (\x -> if x < 0 then V.length vector + x else x)
                <&> min (V.length vector)
            let newVector = V.slice start (max 0 (end - start)) vector
            return (Array newVector)
        _ -> throwErr $ InterpretError (SubscriptNonArray (valueTypeLookup value)) pos
    where
        getLower :: Maybe Exp -> Interpreter Int
        getLower Nothing = return 0
        getLower (Just exp) = do
            value <- eval exp
            case getInt value of
                Just x -> return x
                Nothing -> throwErr $ InterpretError (WrongTypeErr (valueTypeLookup value) "Int") pos
        getHigher :: Maybe Exp -> Interpreter Int
        getHigher Nothing = return maxBound
        getHigher (Just exp) = do
            value <- eval exp
            case getInt value of
                Just x -> return x
                Nothing -> throwErr $ InterpretError (WrongTypeErr (valueTypeLookup value) "Int") pos

eval (Getter exp var pos) = do
    value <- eval exp
    case value of
        DataInstance env -> do
            value <- replaceEnv env $ getterLookup var pos
            case value of
                (Func function) -> do
                    f <- liftIO $ bindSelf env function (DataInstance env)
                    return $ Func f
                _ -> return value
        _ -> do
            newValue <- varLookup var pos
            case newValue of
                (Func function) -> return $ Func (prependArg function value)
                (NativeFunc f) -> do
                    return $ NativeFunc (prependNativeArg f (value, getExpPosition exp))
                _ -> throwErr (InterpretError (callNonFuncErr value) pos)

prependArg :: Function -> Value -> Function
prependArg (Function params stmts env f) arg =
    Function params stmts env $ \(args, pos) -> f (arg:args, pos)

prependNativeArg :: NativeFunction -> (Value, Position) -> NativeFunction
prependNativeArg (NativeFunction arity f) x@(arg, pos) = NativeFunction (arity - 1) new
    where
        ran = runReaderT f :: [(Value, Position)] -> ExceptT Exception IO Value
        new = ReaderT $ \xs -> ran (x:xs)

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
    case find (not . validWhileStmt) stmts of
        Just stmt -> do
            throwErr $ InterpretError (InvalidWhileStmt $ getStmtName stmt) (getStmtPosition stmt)
        Nothing -> return ()
    value <- eval exp
    case value of
        (Bool True) -> do
            env' <- execStmts stmts
            local (const env') (execStmts [While exp stmts pos])
        (Bool False) -> getEnv
        v -> throwErr $ InterpretError (WrongTypeErr (valueTypeLookup v) "Bool") (getExpPosition exp)
    where
        validWhileStmt :: Stmt -> Bool
        validWhileStmt stmt = case stmt of
            LetAssign {} -> False
            VarAssign {} -> False
            FuncDef {} -> False
            DataDef {} -> False
            _ -> True
exec (If [] stmt pos) = execStmts stmt
exec (If ((exp, stmt):xs) stmt' pos) = do
    value <- eval exp
    case value of
        (Bool True) -> execStmts stmt
        (Bool False) -> exec (If xs stmt' pos)
        v -> throwErr $ InterpretError (WrongTypeErr (valueTypeLookup v) "Bool") (getExpPosition exp)
exec (FuncDef s params stmt pos) = do
    env <- getEnv
    scoped <- inScope s
    lift $ except $ guardEither (not scoped) (errWithOffset pos (InvalidRedeclarationOfVar s))
    lift $ except $ guardEither (nubOrd params == params) (errWithOffset pos DuplicateFuncParams)
    let varInits = initVars stmt
    let function = Func $ createRunFunction params stmt env
    addLet (s, function)
exec (DataDef s args stmts pos) = do
    scoped <- inScope s
    lift $ except $ guardEither (not scoped) (errWithOffset pos (InvalidRedeclarationOfVar s))
    case find (not . validClassStmt) stmts of
        Just stmt -> do
            throwErr $ InterpretError (InvalidClassStmt $ getStmtName stmt) (getStmtPosition stmt)
        Nothing -> return ()
    env <- getEnv
    let dataDec = Data args stmts env
    addLet (s, dataDec)
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
    DataDef {} -> True
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
