module Interpret where

import Ast

import Data.Either.Extra
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Data.Maybe (fromJust)

type Prog = Stmt

data Exception = ErrMsg String
    | ReturnExcept Env ExpStmt
    deriving Show

newEnv :: Env
newEnv = Env [] Nothing

envDepth :: Env -> Int
envDepth (Env _ prev) = case prev of
  Nothing -> 0
  Just env -> 1 + envDepth env

replaceEnvDepth :: Env -> Int -> Env -> Env
replaceEnvDepth _ 0 env = env
replaceEnvDepth env@(Env store Nothing) _ _ = env
replaceEnvDepth (Env store (Just prev)) x env = Env store (Just $ replaceEnvDepth prev (x-1) env)

envLookup :: Env -> Var -> Maybe Value
envLookup (Env store Nothing) var = lookup var store
envLookup (Env store (Just prev)) var = case lookup var store of
    (Just val) -> Just val
    Nothing -> envLookup prev var


varLookup :: Env -> Var -> Either Exception Value
varLookup env var = maybeToEither (ErrMsg $ "unbound variable `" ++ var ++ "`") (envLookup env var)

varAssign :: Env -> (Var, Value) -> Env
varAssign (Env vStore prev) var = Env (var : vStore) prev

throwErrIf :: Bool -> String -> Either Exception ()
throwErrIf bool msg = guardEither bool (ErrMsg msg)

throwErr :: String -> Either Exception b
throwErr msg = Left $ ErrMsg msg

varReassign :: Env -> (Var, Value) -> Either Exception Env
varReassign env@(Env vStore prev) pair@(var, val) = case replaceIfExists var vStore val of
    (Just env') -> do
        throwErrIf (envDepth env /= 0) "cannot reassign global variable"
        return (Env env' prev)
    Nothing -> throwErr $ "unbound variable `" ++ var ++ "`"

addScope :: Env -> Env
addScope env = Env [] (Just env)

addVarScope :: Env -> [(Var, Value)] -> Env
addVarScope env vStore = Env vStore (Just env)

removeScope :: Env -> Maybe Env
removeScope (Env _ prev) = prev

replaceVarEnv :: Env -> [(Var, Value)] -> Env
replaceVarEnv (Env _ prev) vStore = Env vStore prev

funcLookup :: Env -> Var -> Either Exception Function
funcLookup env var = case envLookup env var of
    (Just (Func params stmts funcEnv)) -> Right $ Function params stmts funcEnv
    _ -> Left $ ErrMsg $ "no function with name " ++ var ++ " found"

guardEither :: Bool -> a -> Either a ()
guardEither False a = Left a
guardEither True _ = return ()

fixDepth :: Env -> Env -> Env
fixDepth env env' = replaceEnvDepth env (envDepth env - envDepth env') env'

funcCall :: Env -> Var -> [Value] -> ExceptT Exception IO (Env, Value)
funcCall env@(Env store prev) var args = do
    (Function params stmts funcEnv) <- except (funcLookup env var)
    let vars = zip params args
    let env' = addVarScope env vars
    ExceptT $ runExceptT (exec env' stmts) >>= \result -> case result of
            Right env'' -> return $ Right (fromJust $ removeScope env'', Null)
            Left (ReturnExcept env'' expStmt) -> runExceptT $ do
                (env''', val) <- eval env'' expStmt
                return (fromJust $ removeScope env''', val)
            Left a@(_) -> return $ Left a

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


evalExp :: Env -> Exp -> Either Exception Value
evalExp _ (Lit n) = Right n
evalExp env (Var x) = varLookup env x
evalExp env (Add x y) = do
    v1 <- evalExp env x
    v2 <- evalExp env y
    add v1 v2
evalExp env (Sub x y) = do
    v1 <- evalExp env x
    v2 <- evalExp env y
    sub v1 v2
evalExp env (Mul x y) = do
    v1 <- evalExp env x
    v2 <- evalExp env y
    mul v1 v2
evalExp env (Div x y) = do
    v1 <- evalExp env x
    v2 <- evalExp env y
    divide v1 v2
evalExp env (Greater x y) = do
    v1 <- evalExp env x
    v2 <- evalExp env y
    greater v1 v2
evalExp env (Less x y) = do
    v1 <- evalExp env x
    v2 <- evalExp env y
    lesser v1 v2
evalExp env (GreaterEqual x y) = do
    v1 <- evalExp env x
    v2 <- evalExp env y
    greaterEqual v1 v2
evalExp env (LessEqual x y) = do
    v1 <- evalExp env x
    v2 <- evalExp env y
    lesserEqual v1 v2
evalExp env (Equal x y) = do
    v1 <- evalExp env x
    v2 <- evalExp env y
    equal v1 v2
evalExp env (NotEqual x y) = do
    v1 <- evalExp env x
    v2 <- evalExp env y
    notEqual v1 v2
evalExp env (Negate x) = do
    v1 <- evalExp env x
    negateVal v1
evalExp env (Bang x) = do
    v1 <- evalExp env x
    bang v1

eval :: Env -> ExpStmt -> ExceptT Exception IO (Env, Value)
eval env (Expr exp) = except $ do
    value <- evalExp env exp
    return (env, value)
eval env (CallFunc name args) = do
    (env', vals) <- evalArgs env args
    funcCall env' name vals
    where
        evalArgs :: Env -> [ExpStmt] -> ExceptT Exception IO (Env, [Value])
        evalArgs env [] = return (env, [])
        evalArgs env (x:xs) = do
            (env', val) <- eval env x
            (env'', vals) <- evalArgs env' xs
            return (env'', val:vals)

exec :: Env -> Stmt -> ExceptT Exception IO Env
exec env (VarAssign s exp) = do
    (env', value) <- eval env exp
    return $ varAssign env' (s, value)
exec env (VarReassign s exp) = do
    (env', value) <- eval env exp
    except $ varReassign env' (s, value)
exec env (While exp stmt) = do
    (env', value) <- eval env exp
    case value of
        (Bool True) -> exec env' (Seq [stmt, While exp stmt])
        (Bool False) -> return env'
        v -> throwE $ ErrMsg $ genericTypeException (valueTypeLookup v) "Bool"
exec env (If [] stmt) = exec env stmt
exec env (If ((exp, stmt):xs) stmt') = do
    (env', value) <- eval env exp
    case value of
        (Bool True) -> exec env' (Seq [stmt, If xs stmt'])
        (Bool False) -> exec env' $ If xs stmt'
        v -> throwE $ ErrMsg $ genericTypeException (valueTypeLookup v) "Bool"
exec env (Seq []) = return env
exec env (Seq (x:xs)) = exec env x >>= \env' -> exec env' (Seq xs)
exec env (FuncDef s args stmt) =
    let function = Func args stmt env
    in return $ varAssign env (s, function)
exec env (CallExpStmt expStmt) = case expStmt of
    (Expr exp) -> throwE $ ErrMsg "unused result of expression"
    _ -> fst <$> eval env expStmt
exec env (ReturnStmt expStmt) = throwE $ ReturnExcept env expStmt
exec env (Print expStmt) = do
    (env', val) <- eval env expStmt
    lift (printVal val)
    return env'


runProgram :: Env -> Stmt -> IO ()
runProgram env stmt = do
    result <- runExceptT $ exec env stmt
    case result of
        Right _ -> return ()
        Left (ReturnExcept _ _) -> putStrLn "unexpected return statement not nested in function"
        Left (ErrMsg msg) -> putStrLn msg

execNew :: Stmt -> IO ()
execNew stmt = runProgram newEnv stmt

isVar :: Value -> Bool
isVar Func {} = False
isVar _ = True

testAst :: IO ()
testAst = (execNew $
    Seq [
        VarAssign "x" (Expr $ Lit $ Float 5),
        FuncDef "f" [] $
        Seq [
            VarAssign "x" (Expr $ Lit $ Float 5)
        ],
        CallExpStmt $ CallFunc "f" []
    ])

--testAst :: Either String Env
--testAst = --(filter (\(var, val) -> isVar val)) <$> varEnv <$> 
--  (execNew (
--    Seq [
----        VarAssign "x" (Expr $ Lit $ Num 5), 
----        VarReassign "x" (Expr $ Lit $ Bool True),
--        FuncDef "makeCounter" [] (Seq [
----            VarAssign "i" (Expr $ Lit $ Num 0),
--            FuncDef "count" [] (Seq [
----                VarReassign "i" (Expr $ Add (Var "i") (Lit $ Num 1)),
----                VarReassign "x" (Expr $ Add (Var "x") (Var "i"))
--            ]),
----            CallExpStmt $ CallFunc "count" [],
----            VarAssign "y" (Expr $ Var "count"),
--            ReturnStmt (Expr $ Var "count")
--        ]),
--        VarAssign "y" (CallFunc "makeCounter" []) --[Expr $ Lit $ Num 3]),
----        CallExpStmt (CallFunc "y" [])
--        -- VarAssign "counter" (CallProc "makeCounter" [])
--        -- ProcDef "function" [] (Seq [
--        --     VarReassign "x" (Expr $ Lit $ Bool False),
--        --     VarAssign "x" (Expr $ Lit $ Float 63.2),
--        --     VarAssign "y" (Expr $ Lit $ Float 5.52),
--        --     ReturnStmt (Expr $ Lit $ Float 4.3)
--        -- ]),
--        -- VarAssign "return" (CallProc "function" [])
--        
--        ]
--    ))

