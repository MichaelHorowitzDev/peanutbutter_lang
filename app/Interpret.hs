module Interpret where

import Ast

import Data.Either.Extra
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Data.Maybe (fromJust)
import Data.IORef

type Prog = Stmt

data Exception = ErrMsg String
    | ReturnExcept Env Exp
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


varLookup :: Env -> Var -> ExceptT Exception IO Value
varLookup env var = do
    result <- lift $ envLookup env var
    case value <$> result of
        Nothing -> throwE $ ErrMsg $ "unbound variable `" ++ var ++ "`"
        (Just Null) -> throwE $ ErrMsg $ "attempt to reference variable `" ++ var ++ "` before it was initialized"
        (Just x) -> return x

member :: Eq a => a -> [(a, b)] -> Bool
member a = foldr (\x acc -> a == fst x || acc) False

inScope :: Var -> Env -> IO Bool
inScope v (Env store _) = do
    store <- readIORef store
    case lookup v store of
        (Just val) -> return $ not $ isNull (value val)
        Nothing -> return False

varAssign :: Env -> (Var, Value) -> IO Env
varAssign (Env vStore prev) (var, val) = do
    let value = Val {value=val, mutable=True}
    modifyIORef vStore ((var, value):)
    return $ Env vStore prev

letAssign :: Env -> (Var, Value) -> IO Env
letAssign (Env vStore prev) (var, val) = do
    let value = Val {value=val, mutable=False}
    modifyIORef vStore ((var, value):)
    return $ Env vStore prev

throwErrIf :: Bool -> String -> Either Exception ()
throwErrIf bool msg = guardEither (not bool) (ErrMsg msg)

throwErr :: String -> Either Exception b
throwErr msg = Left $ ErrMsg msg

varReassign :: Env -> (Var, Value) -> ExceptT Exception IO Env
varReassign env@(Env vStore prev) pair@(var, val) = do
    result <- lift $ envLookup env var
    case result of
        Nothing -> throwE $ ErrMsg $ "unbound variable `" ++ var ++ "`"
        (Just val) -> do
            except $ throwErrIf (not $ mutable val) "cannot reassign immutable value"
            case value val of
                Null -> throwE $ ErrMsg $ "attempt to reference variable `" ++ var ++ "` before it was initialized"

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

funcLookup :: Env -> Var -> ExceptT Exception IO Function
funcLookup env var = do
    result <- lift $ envLookup env var 
    case value <$> result of
        (Just (Func params stmts funcEnv)) -> return $ Function params stmts funcEnv
        Nothing -> throwE $ ErrMsg $ "no function with name " ++ var ++ " found"

guardEither :: Bool -> a -> Either a ()
guardEither False a = Left a
guardEither True _ = return ()

fixDepth :: Env -> Env -> Env
fixDepth env env' = replaceEnvDepth env (envDepth env - envDepth env') env'

funcCall :: Env -> Function -> [Value] -> ExceptT Exception IO Value
funcCall env@(Env store prev) function args = do
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
            (ErrMsg $ "incorrect number of arguments passed to function" ++
            "\n" ++ show params ++ " parameters expected but " ++ show args ++ " arguments passed in")
            where
                params = length xs
                args = length ys

replaceIfExists :: (Eq a) => a -> [(a, b)] -> b -> Maybe [(a, b)]
replaceIfExists _ [] _ = Nothing
replaceIfExists key (x:xs) value
    | key == fst x = Just $ (key, value) : xs
    | otherwise = (x :) <$> replaceIfExists key xs value

genericTypeException :: String -> String -> String
genericTypeException x y =
    "cannot use value of type `" ++ x ++ "` where value of type `" ++ y ++ "` was expected"

operationTypeError :: String -> Value -> Value -> Either Exception b
operationTypeError op x y =
    let v1 = valueTypeLookup x
        v2 = valueTypeLookup y
    in Left $ ErrMsg $ "cannot " ++ op ++ " values of type `" ++ v1 ++ "` and type `" ++ v2 ++ "`"

operationTypeErrorSingle :: String -> Value -> Either Exception b
operationTypeErrorSingle op x =
    let v1 = valueTypeLookup x
    in Left $ ErrMsg $ "cannot " ++ op ++ " value of type `" ++ v1 ++ "`"

add :: Value -> Value -> Either Exception Value
add (Int x) (Int y) = Right $ Int (x + y)
add (Float x) (Float y) = Right $ Float (x + y)
add x y = operationTypeError "add" x y

sub :: Value -> Value -> Either Exception Value
sub (Int x) (Int y) = Right $ Int (x - y)
sub (Float x) (Float y) = Right $ Float (x - y)
sub x y = operationTypeError "subtract" x y

mul :: Value -> Value -> Either Exception Value
mul (Int x) (Int y) = Right $ Int (x * y)
mul (Float x) (Float y) = Right $ Float (x * y)
mul x y = operationTypeError "multiply" x y

divide :: Value -> Value -> Either Exception Value
divide (Int x) (Int y) = Right $ Int (x `div` y)
divide (Float x) (Float y) = Right $ Float (x / y)
divide x y = operationTypeError "divide" x y

greater :: Value -> Value -> Either Exception Value
greater (Int x) (Int y) = return $ Bool $ x > y
greater (Float x) (Float y) = return $ Bool $ x > y
greater x y = operationTypeError "compare" x y

greaterEqual :: Value -> Value -> Either Exception Value
greaterEqual (Int x) (Int y) = return $ Bool $ x >= y
greaterEqual (Float x) (Float y) = return $ Bool $ x >= y
greaterEqual x y = operationTypeError "compare" x y

lesser :: Value -> Value -> Either Exception Value
lesser (Int x) (Int y) = return $ Bool $ x < y
lesser (Float x) (Float y) = return $ Bool $ x < y
lesser x y = operationTypeError "compare" x y

lesserEqual :: Value -> Value -> Either Exception Value
lesserEqual (Int x) (Int y) = return $ Bool $ x <= y
lesserEqual (Float x) (Float y) = return $ Bool $ x <= y
lesserEqual x y = operationTypeError "compare" x y

equal :: Value -> Value -> Either Exception Value
equal (Int x) (Int y) = return $ Bool $ x == y
equal (Float x) (Float y) = return $ Bool $ x == y
equal (String x) (String y) = return $ Bool $ x == y
equal (Bool x) (Bool y) = return $ Bool $ x == y
equal x y = operationTypeError "compare" x y

notEqual :: Value -> Value -> Either Exception Value
notEqual (Int x) (Int y) = return $ Bool $ x /= y
notEqual (Float x) (Float y) = return $ Bool $ x /= y
notEqual (String x) (String y) = return $ Bool $ x /= y
notEqual (Bool x) (Bool y) = return $ Bool $ x /= y
notEqual x y = operationTypeError "compare" x y

negateVal :: Value -> Either Exception Value
negateVal (Int x) = return $ Int (negate x)
negateVal (Float x) = return $ Float (negate x)
negateVal x = operationTypeErrorSingle "negate" x

bang :: Value -> Either Exception Value
bang (Bool b) = return $ Bool (not b)
bang x = operationTypeErrorSingle "invert" x

printVal :: Value -> IO ()
printVal (String s) = putStrLn s
printVal (Float f) = print f
printVal (Int n) = print n
printVal (Bool b) = putStrLn (if b then "true" else "false")
printVal (Func {}) = putStrLn "<func>"
printVal Null = putStrLn "Null"

eval :: Env -> Exp -> ExceptT Exception IO Value
eval env (CallFunc exp args) = do
    value <- eval env exp
    case value of
        (Func params stmts funcEnv) -> do
            args <- evalArgs env args
            funcCall env (Function params stmts funcEnv) args
        _ -> throwE $ ErrMsg $ "cannot call value of non function type `" ++ valueTypeLookup value ++ "`"
    where
        evalArgs :: Env -> [Exp] -> ExceptT Exception IO [Value]
        evalArgs env [] = return []
        evalArgs env (x:xs) = do
            val <- eval env x
            vals <- evalArgs env xs
            return (val:vals)
eval env (Lambda params exp) = do
    let function = Func params (ReturnStmt exp) env
    return function
eval _ (Lit n) = return n
eval env (Var x) = varLookup env x
eval env (Add x y) = do
    v1 <- eval env x
    v2 <- eval env y
    except $ add v1 v2
eval env (Sub x y) = do
    v1 <- eval env x
    v2 <- eval env y
    except $ sub v1 v2
eval env (Mul x y) = do
    v1 <- eval env x
    v2 <- eval env y
    except $ mul v1 v2
eval env (Div x y) = do
    v1 <- eval env x
    v2 <- eval env y
    except $ divide v1 v2
eval env (Greater x y) = do
    v1 <- eval env x
    v2 <- eval env y
    except $ greater v1 v2
eval env (Less x y) = do
    v1 <- eval env x
    v2 <- eval env y
    except $ lesser v1 v2
eval env (GreaterEqual x y) = do
    v1 <- eval env x
    v2 <- eval env y
    except $ greaterEqual v1 v2
eval env (LessEqual x y) = do
    v1 <- eval env x
    v2 <- eval env y
    except $ lesserEqual v1 v2
eval env (Equal x y) = do
    v1 <- eval env x
    v2 <- eval env y
    except $ equal v1 v2
eval env (NotEqual x y) = do
    v1 <- eval env x
    v2 <- eval env y
    except $ notEqual v1 v2
eval env (Negate x) = do
    v1 <- eval env x
    except $ negateVal v1
eval env (Bang x) = do
    v1 <- eval env x
    except $ bang v1

getVarDecs :: Stmt -> [Var]
getVarDecs (VarAssign s _) = [s]
getVarDecs (Seq []) = []
getVarDecs (Seq (x:xs)) = getVarDecs x ++ getVarDecs (Seq xs)
getVarDecs _ = []

initVars :: Stmt -> Stmt
initVars stmt = Seq $ map (`VarAssign` Lit Null) (getVarDecs stmt) ++ [stmt]

moveFuncDecs :: Stmt -> Stmt
moveFuncDecs stmt =
    let (funcs, rest) = partition stmt
    in Seq $ funcs ++ rest
    where
        partition :: Stmt -> ([Stmt], [Stmt])
        partition func@(FuncDef {}) = ([func], [])
        partition (Seq []) = ([], [])
        partition (Seq (x:xs)) =
            let (funcs, rest) = partition x
                (funcs', rest') = partition (Seq xs)
            in (funcs ++ funcs', rest ++ rest')
        partition stmt = ([], [stmt])

exec :: Env -> Stmt -> ExceptT Exception IO Env
exec env (VarAssign s exp) = do
    scoped <- lift (s `inScope` env)
    except $ throwErrIf scoped ("invalid redeclaration of `" ++ s ++ "`")
    value <- eval env exp
    lift $ varAssign env (s, value)
exec env (LetAssign s exp) = do
    scoped <- lift (s `inScope` env)
    except $ throwErrIf scoped ("invalid redeclaration of `" ++ s ++ "`")
    value <- eval env exp
    lift $ letAssign env (s, value)
exec env (VarReassign s exp) = do
    value <- eval env exp
    varReassign env (s, value)
exec env (While exp stmt) = do
    value <- eval env exp
    case value of
        (Bool True) -> exec env (Seq [stmt, While exp stmt])
        (Bool False) -> return env
        v -> throwE $ ErrMsg $ genericTypeException (valueTypeLookup v) "Bool"
exec env (If [] stmt) = exec env stmt
exec env (If ((exp, stmt):xs) stmt') = do
    value <- eval env exp
    case value of
        (Bool True) -> exec env stmt
        (Bool False) -> exec env $ If xs stmt'
        v -> throwE $ ErrMsg $ genericTypeException (valueTypeLookup v) "Bool"
exec env (Seq []) = return env
exec env (Seq (x:xs)) = exec env x >>= \env' -> exec env' (Seq xs)
exec env (FuncDef s args stmt) = do
    scoped <- lift (s `inScope` env)
    except $ throwErrIf scoped ("invalid redeclaration of `" ++ s ++ "`")
    let movedDecs = moveFuncDecs stmt
    let varInits = initVars movedDecs
    let function = Func args varInits env
    lift $ varAssign env (s, function)
exec env (CallExp exp) = case exp of
    (CallFunc name args) -> eval env exp >> return env
    _ -> eval env exp >> return env
exec env (ReturnStmt expStmt) = throwE $ ReturnExcept env expStmt
exec env (Print expStmt) = do
    val <- eval env expStmt
    lift (printVal val)
    return env

performTransformations :: Stmt -> Stmt
performTransformations = initVars . moveFuncDecs

runProgram :: Env -> Stmt -> IO ()
runProgram env stmt = do
    let transformed = performTransformations stmt
    result <- runExceptT $ exec env transformed
    case result of
        Right _ -> return ()
        Left (ReturnExcept _ _) -> putStrLn "unexpected return statement not nested in function"
        Left (ErrMsg msg) -> putStrLn msg

execNew :: Stmt -> IO ()
execNew stmt = newEnv >>= \env -> runProgram env stmt

isVar :: Value -> Bool
isVar Func {} = False
isVar _ = True
