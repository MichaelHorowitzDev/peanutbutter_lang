module Interpret where

import Ast

import Data.Either.Extra
import Control.Monad
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

funcCall :: Env -> Var -> [Value] -> Either Exception (Env, Value)
funcCall env@(Env store prev) var args = do
    (Function params stmts funcEnv) <- funcLookup env var
    let vars = zip params args
    let env' = addVarScope env vars
    case exec env' stmts of
        Right env'' -> Right (fromJust $ removeScope env'', Null)
        Left (ReturnExcept env'' expStmt) -> do
            (env''', val) <- eval env'' expStmt
            Right (fromJust $ removeScope env''', val)
        Left a@(_) -> Left a
    
replaceIfExists :: (Eq a) => a -> [(a, b)] -> b -> Maybe [(a, b)]
replaceIfExists _ [] _ = Nothing
replaceIfExists key (x:xs) value
    | key == fst x = Just $ (key, value) : xs
    | otherwise = (x :) <$> replaceIfExists key xs value

genericTypeException :: String -> String -> String
genericTypeException x y = 
    "cannot use value of type `" ++ x ++ "` where value of type `" ++ y ++ "` was expected"

add :: Value -> Value -> Either Exception Value
--add (Num x) (Num y) = Right $ Num (x + y)
add (Float x) (Float y) = Right $ Float (x + y)
add x y =
    let v1 = valueTypeLookup x
        v2 = valueTypeLookup y
    in Left $ ErrMsg $ "cannot add values of type `" ++ v1 ++ "` and type `" ++ v2 ++ "`"

sub :: Value -> Value -> Either Exception Value
--sub (Num x) (Num y) = Right $ Num (x - y)
sub (Float x) (Float y) = Right $ Float (x - y)
sub x y =
    let v1 = valueTypeLookup x
        v2 = valueTypeLookup y
    in Left $ ErrMsg $ "cannot subtract values of type `" ++ v1 ++ "` and type `" ++ v2 ++ "`"

mul :: Value -> Value -> Either Exception Value
--mul (Num x) (Num y) = Right $ Num (x * y)
mul (Float x) (Float y) = Right $ Float (x * y)
mul x y =
    let v1 = valueTypeLookup x
        v2 = valueTypeLookup y
    in Left $ ErrMsg $ "cannot multiply values of type `" ++ v1 ++ "` and type `" ++ v2 ++ "`"

divide :: Value -> Value -> Either Exception Value
--divide (Num x) (Num y) = Right $ Num (x `div` y)
divide (Float x) (Float y) = Right $ Float (x / y)
divide x y =
    let v1 = valueTypeLookup x
        v2 = valueTypeLookup y
    in Left $ ErrMsg $ "cannot divide values of type `" ++ v1 ++ "` and type `" ++ v2 ++ "`"

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

eval :: Env -> ExpStmt -> Either Exception (Env, Value)
eval env (Expr exp) = do
    value <- evalExp env exp
    return (env, value)
eval env (CallFunc name args) = do
    (env', vals) <- evalArgs env args
    funcCall env' name vals
    where
        evalArgs :: Env -> [ExpStmt] -> Either Exception (Env, [Value])
        evalArgs env [] = Right (env, [])
        evalArgs env (x:xs) = do
            (env', val) <- eval env x
            (env'', vals) <- evalArgs env' xs
            return (env'', val:vals)    

exec :: Env -> Stmt -> Either Exception Env
exec env (VarAssign s exp) = do
    (env', value) <- eval env exp
    return $ varAssign env' (s, value)
exec env (VarReassign s exp) = do
    (env', value) <- eval env exp
    varReassign env' (s, value)
exec env (While exp stmt) = do
    (env', value) <- eval env exp
    case value of
        (Bool True) -> exec env' (Seq [stmt, While exp stmt])
        (Bool False) -> Right env'
        v -> Left $ ErrMsg $ genericTypeException (valueTypeLookup v) "Bool"
exec env (Seq []) = Right env
exec env (Seq (x:xs)) = exec env x >>= \env' -> exec env' (Seq xs)
exec env (FuncDef s args stmt) = 
    let function = Func args stmt env
    in Right $ varAssign env (s, function)
exec env (CallExpStmt expStmt) = case expStmt of
    (Expr exp) -> Left $ ErrMsg "unused result of expression"
    _ -> fst <$> eval env expStmt
exec env (ReturnStmt expStmt) = Left $ ReturnExcept env expStmt

runProgram :: Env -> Stmt -> Either String Env
runProgram env stmt = case exec env stmt of
    (Right env') -> Right env'
    (Left (ReturnExcept _ _)) -> Left "unexpected return statement not nested in function"
    (Left (ErrMsg msg)) -> Left msg

execNew :: Stmt -> Either String Env
execNew stmt = runProgram newEnv stmt

isVar :: Value -> Bool
isVar Func {} = False
isVar _ = True

--testAst :: Either String Env
testAst = (filter (isVar . snd) . varEnv) <$> (execNew $
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

