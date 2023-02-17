module Interpret where

import Ast

import Data.Either.Extra
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Data.Maybe (fromJust)
import Data.IORef
import Text.Megaparsec

type Prog = Stmt

data Exception = ErrMsg String
    | ReturnExcept Env Exp
    | InterpErr InterpretError
    deriving Show

newEnv :: IO Env
newEnv = do
    x <- newIORef []
    return $ Env x Nothing

envDepth :: Env -> Int
envDepth (Env _ prev) = case prev of
  Nothing -> 0
  Just env -> 1 + envDepth env

replaceEnvDepth :: Env -> Int -> Env -> Env
replaceEnvDepth _ 0 env = env
replaceEnvDepth env@(Env store Nothing) _ _ = env
replaceEnvDepth (Env store (Just prev)) x env = Env store (Just $ replaceEnvDepth prev (x-1) env)

envLookup :: Env -> Var -> IO (Maybe Val)
envLookup (Env store Nothing) var = readIORef store >>= return . lookup var
envLookup (Env store (Just prev)) var = readIORef store >>= \s -> case lookup var s of
    (Just val) -> return $ Just val
    Nothing -> envLookup prev var

varLookup :: Env -> Var -> Position -> ExceptT Exception IO Value
varLookup env var offset = do
    result <- lift $ envLookup env var
    val <- except $ maybeToEither (throwWithOffset offset (UnboundErr var)) result
    except $ throwErrIf' (isNull $ value val) (InterpretError (RefBeforeInit var) offset)

    scoped <- lift $ var `inScope` env
    except $ throwErrIf' (not scoped && mutable val) (InterpretError (RefMutVar var) offset)
    
    return $ value val

member :: Eq a => a -> [(a, b)] -> Bool
member a = foldr (\x acc -> a == fst x || acc) False

inScope :: Var -> Env -> IO Bool
inScope v (Env store _) = do
    store <- readIORef store
    case lookup v store of
        (Just val) -> return $ not $ isNull (value val)
        Nothing -> return False

addVar :: Env -> (Var, Value) -> IO Env
addVar (Env vStore prev) (var, val) = do
    let value = Val {value=val, mutable=True}
    modifyIORef vStore ((var, value):)
    return $ Env vStore prev

addLet :: Env -> (Var, Value) -> IO Env
addLet (Env vStore prev) (var, val) = do
    let value = Val {value=val, mutable=False}
    modifyIORef vStore ((var, value):)
    return $ Env vStore prev

throwErrIf :: Bool -> String -> Either Exception ()
throwErrIf bool msg = guardEither (not bool) (ErrMsg msg)

throwErr :: String -> Either Exception b
throwErr msg = Left $ ErrMsg msg

varReassign :: Env -> (Var, Value) -> Position -> ExceptT Exception IO Env
varReassign env@(Env vStore prev) pair@(var, val) pos = do
    result <- lift $ envLookup env var
    val <- except $ maybeToEither (throwWithOffset pos (UnboundErr var)) result
    except $ guardEither (mutable val) (throwWithOffset pos (ReassignImmutableErr var))
    except $ guardEither (not $ isNull (value val)) (throwWithOffset pos (RefBeforeInit var))
    lift $ reassign env pair
    where
        reassign :: Env -> (Var, Value) -> IO Env
        reassign env@(Env vStore prev) (var, val) = do
            store <- readIORef vStore
            case replaceIfExists var store (mutableVar val) of
                (Just new) -> do
                    writeIORef vStore new
                    return (Env vStore prev)
                Nothing -> case prev of
                    Nothing -> return env
                    (Just env') -> do
                        env'' <- reassign env' (var, val)
                        return (Env vStore (Just env''))

addScope :: Env -> IO Env
addScope env = do
    x <- newIORef []
    return $ Env x (Just env)

immutableVar :: Value -> Val
immutableVar v = Val {value=v, mutable=False}

mutableVar :: Value -> Val
mutableVar v = Val {value=v, mutable=True}

addConstScope :: Env -> [(Var, Value)] -> IO Env
addConstScope env store = do
    let consts = map (\(x, y) -> (x, immutableVar y)) store
    vars <- newIORef consts
    return $ Env vars (Just env)

removeScope :: Env -> Maybe Env
removeScope (Env _ prev) = prev

guardEither :: Bool -> a -> Either a ()
guardEither False a = Left a
guardEither True _ = return ()

fixDepth :: Env -> Env -> Env
fixDepth env env' = replaceEnvDepth env (envDepth env - envDepth env') env'

funcCall :: Env -> Function -> [Value] -> Position -> ExceptT Exception IO Value
funcCall env@(Env store prev) function args pos = do
    let (Function params stmts funcEnv) = function
    except $ testArity params args
    let vars = zip params args
    env' <- lift $ addConstScope funcEnv vars
    ExceptT $ runExceptT (exec env' stmts) >>= \result -> case result of
            Right env'' -> return $ Right Null
            Left (ReturnExcept env'' expStmt) -> runExceptT $ eval env'' expStmt
            Left a -> return $ Left a
    where
        testArity :: [String] -> [Value] -> Either Exception ()
        testArity xs ys = guardEither (params == args)
            (throwWithOffset pos (ArityErr params args))
            where
                params = length xs
                args = length ys

replaceIfExists :: (Eq a) => a -> [(a, b)] -> b -> Maybe [(a, b)]
replaceIfExists _ [] _ = Nothing
replaceIfExists key (x:xs) value
    | key == fst x = Just $ (key, value) : xs
    | otherwise = (x :) <$> replaceIfExists key xs value

add :: Value -> Value -> Position -> Either Exception Value
add x y pos = case (x, y) of
    (Int x, Int y) -> return $ Int (x + y)
    (Float x, Float y) -> return $ Float (x + y)
    _ -> Left $ throwWithOffset pos (binOpTypeErr "add" x y)

sub :: Value -> Value -> Position -> Either Exception Value
sub x y pos = case (x, y) of
    (Int x, Int y) -> return $ Int (x - y)
    (Float x, Float y) -> return $ Float (x - y)
    _ -> Left $ throwWithOffset pos (binOpTypeErr "subtract" x y)

mul :: Value -> Value -> Position -> Either Exception Value
mul (Int x) (Int y) pos = Right $ Int (x * y)
mul (Float x) (Float y) pos = Right $ Float (x * y)
mul x y pos = Left $ throwWithOffset pos (binOpTypeErr "multiply" x y)

divide :: Value -> Value -> Position -> Either Exception Value
divide x y pos = case (x, y) of
    (Int x, Int y) -> return $ Int (x `div` y)
    (Float x, Float y) -> return $ Float (x / y)
    _ -> Left $ throwWithOffset pos (binOpTypeErr "divide" x y)

greater :: Value -> Value -> Position -> Either Exception Value
greater (Int x) (Int y) pos = return $ Bool $ x > y
greater (Float x) (Float y) pos = return $ Bool $ x > y
greater x y pos = Left $ throwWithOffset pos (binOpTypeErr "compare" x y)

greaterEqual :: Value -> Value -> Position -> Either Exception Value
greaterEqual x y pos = case (x, y) of
    (Int x, Int y) -> return $ Bool $ x >= y
    (Float x, Float y) -> return $ Bool $ x >= y
    _ -> Left $ throwWithOffset pos (binOpTypeErr "compare" x y)

lesser :: Value -> Value -> Position -> Either Exception Value
lesser x y pos = case (x, y) of
    (Int x, Int y) -> return $ Bool $ x < y
    (Float x, Float y) -> return $ Bool $ x < y
    _ -> Left $ throwWithOffset pos (binOpTypeErr "compare" x y)

lesserEqual :: Value -> Value -> Position -> Either Exception Value
lesserEqual x y pos = case (x, y) of
    (Int x, Int y) -> return $ Bool $ x <= y
    (Float x, Float y) -> return $ Bool $ x <= y
    _ -> Left $ throwWithOffset pos (binOpTypeErr "compare" x y)

equal :: Value -> Value -> Position -> Either Exception Value
equal x y pos = case (x, y) of
    (Int x, Int y) -> return $ Bool $ x == y 
    (Float x, Float y) -> return $ Bool $ x == y
    (String x, String y) -> return $ Bool $ x == y
    (Bool x, Bool y) -> return $ Bool $ x == y
    _ -> Left $ throwWithOffset pos (binOpTypeErr "compare" x y)

notEqual :: Value -> Value -> Position -> Either Exception Value
notEqual x y pos = case (x, y) of
    (Int x, Int y) -> return $ Bool $ x /= y
    (Float x, Float y) -> return $ Bool $ x /= y
    (String x, String y) -> return $ Bool $ x /= y
    (Bool x, Bool y) -> return $ Bool $ x /= y
    _ -> Left $ throwWithOffset pos (binOpTypeErr "compare" x y)

negateVal :: Value -> Position -> Either Exception Value
negateVal x pos = case x of
    (Int x) -> return $ Int (negate x)
    (Float x) -> return $ Float (negate x)
    _ -> Left $ throwWithOffset pos (unOpTypeErr "negate" x)

bang :: Value -> Position -> Either Exception Value
bang x pos = case x of
    (Bool x) -> return $ Bool (not x)
    _ -> Left $ throwWithOffset pos (unOpTypeErr "negate" x)

printVal :: Value -> IO ()
printVal (String s) = putStrLn s
printVal (Float f) = print f
printVal (Int n) = print n
printVal (Bool b) = putStrLn (if b then "true" else "false")
printVal (Func {}) = putStrLn "<func>"
printVal Null = putStrLn "Null"

eval :: Env -> Exp -> ExceptT Exception IO Value
eval _ (Lit n pos) = return n
eval env (Var s pos) = varLookup env s pos
eval env (Add x y pos) = do
    v1 <- eval env x
    v2 <- eval env y
    except $ add v1 v2 pos
eval env (Sub x y pos) = do
    v1 <- eval env x
    v2 <- eval env y
    except $ sub v1 v2 pos
eval env (Mul x y pos) = do
    v1 <- eval env x
    v2 <- eval env y
    except $ mul v1 v2 pos
eval env (Div x y pos) = do
    v1 <- eval env x
    v2 <- eval env y
    except $ divide v1 v2 pos
eval env (Greater x y pos) = do
    v1 <- eval env x
    v2 <- eval env y
    except $ greater v1 v2 pos
eval env (Less x y pos) = do
    v1 <- eval env x
    v2 <- eval env y
    except $ lesser v1 v2 pos
eval env (GreaterEqual x y pos) = do
    v1 <- eval env x
    v2 <- eval env y
    except $ greaterEqual v1 v2 pos
eval env (LessEqual x y pos) = do
    v1 <- eval env x
    v2 <- eval env y
    except $ lesserEqual v1 v2 pos
eval env (Equal x y pos) = do
    v1 <- eval env x
    v2 <- eval env y
    except $ equal v1 v2 pos
eval env (NotEqual x y pos) = do
    v1 <- eval env x
    v2 <- eval env y
    except $ notEqual v1 v2 pos
eval env (Negate x pos) = do
    v1 <- eval env x
    except $ negateVal v1 pos
eval env (Bang x pos) = do
    v1 <- eval env x
    except $ bang v1 pos
eval env (CallFunc exp args pos) = do
    value <- eval env exp
    case value of
        (Func params stmts funcEnv) -> do
            args <- evalArgs env args
            funcCall env (Function params stmts funcEnv) args pos
        _ -> throwE $ throwWithOffset pos (callNonFuncErr value)
    return value
    where
        evalArgs :: Env -> [Exp] -> ExceptT Exception IO [Value]
        evalArgs env [] = return []
        evalArgs env (x:xs) = do
            val <- eval env x
            vals <- evalArgs env xs
            return (val:vals)
eval env (Lambda params exp pos) = do
    let function = Func params (ReturnStmt exp (Position 0 0)) env
    return function

initVars :: Stmt -> Stmt
initVars stmt = Seq (getDecs stmt ++ [stmt])
    where
        getDecs :: Stmt -> [Stmt]
        getDecs (VarAssign s _ _) = [VarAssign s (Lit Null (Position 0 0)) (Position 0 0)]
        getDecs (LetAssign s _ _) = [LetAssign s (Lit Null (Position 0 0)) (Position 0 0)]
        getDecs (Seq []) = []
        getDecs (Seq (x:xs)) = getDecs x ++ getDecs (Seq xs)
        getDecs _ = []

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

throwWithOffset :: Position -> InterpretErrorType -> Exception
throwWithOffset offset errType =
    let interpretError = InterpretError errType offset
    in InterpErr interpretError

throwErrIf' :: Bool -> InterpretError -> Either Exception ()
throwErrIf' False _ = return ()
throwErrIf' True err = Left $ InterpErr err

exec :: Env -> Stmt -> ExceptT Exception IO Env
exec env (VarAssign s exp pos) = do
    scoped <- lift (s `inScope` env)
    except $ guardEither (not scoped) (throwWithOffset pos (InvalidRedeclarationOfVar s))
    value <- eval env exp
    lift $ addVar env (s, value)
exec env (LetAssign s exp pos) = do
    scoped <- lift (s `inScope` env)
    except $ guardEither (not scoped) (throwWithOffset pos (InvalidRedeclarationOfVar s))
    value <- eval env exp
    lift $ addLet env (s, value)
exec env (VarReassign s exp pos) = do
    value <- eval env exp
    varReassign env (s, value) pos
exec env (While exp stmt pos) = do
    value <- eval env exp
    case value of
        (Bool True) -> exec env (Seq [stmt, While exp stmt pos])
        (Bool False) -> return env
        v -> throwE $ throwWithOffset (getExpPosition exp) (WrongTypeErr (valueTypeLookup v) "Bool")
exec env (If [] stmt pos) = exec env stmt
exec env (If ((exp, stmt):xs) stmt' pos) = do
    value <- eval env exp
    case value of
        (Bool True) -> exec env stmt
        (Bool False) -> exec env $ If xs stmt' pos
        v -> throwE $ throwWithOffset (getExpPosition exp) (WrongTypeErr (valueTypeLookup v) "Bool")
    return env
exec env (Seq []) = return env
exec env (Seq (x:xs)) = exec env x >>= \env' -> exec env' (Seq xs)
exec env (FuncDef s args stmt pos) = do
    scoped <- lift (s `inScope` env)
    except $ guardEither (not scoped) (throwWithOffset pos (InvalidRedeclarationOfVar s))
    let varInits = initVars stmt
    let function = Func args varInits env
    lift $ addLet env (s, function)
exec env (CallExp exp pos) = case exp of
    (CallFunc name args pos) -> eval env exp >> return env
    _ -> eval env exp >> return env
exec env (ReturnStmt expStmt pos) = throwE $ ReturnExcept env expStmt
exec env (Print exp pos) = do
    val <- eval env exp
    lift (printVal val)
    return env

performTransformations :: Stmt -> Stmt
performTransformations = initVars

runProgram :: Env -> Stmt -> ExceptT InterpretError IO ()
runProgram env stmt = ExceptT $ do
    let transformed = performTransformations stmt
    result <- runExceptT $ exec env transformed
    case result of
        Right _ -> return $ return ()
        Left (InterpErr err) -> return $ Left err
        Left (ReturnExcept _ _) -> putStrLn "unexpected return statement not nested in function" >> return (return ())
        Left (ErrMsg msg) -> putStrLn msg >> return (return ())

execNew :: Stmt -> ExceptT InterpretError IO ()
execNew stmt = lift newEnv >>= \env -> runProgram env stmt

isVar :: Value -> Bool
isVar Func {} = False
isVar _ = True
