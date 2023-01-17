module Interpret where

import Ast

import Data.IORef
import System.IO.Unsafe

type Var = String
type Store = [(Var, Value)]
-- type Proc = ([String], Stmt)

type Prog = Stmt

instance (Show a) => Show (IORef a) where
    show a = show (unsafePerformIO (readIORef a))

-- data Env = Env { 
--     varIOEnv :: IORef [(Var, Value)], 
--     -- procEnv :: [(Var, Proc)], 
--     prevIOEnv :: Maybe Env 
--     }
--     deriving Show

data Env = Env {
    varEnv :: [(Var, Value)],
    prevEnv :: Maybe Env
}
    deriving Show
        
data ReadEnv = ReadEnv {
    varReadEnv :: [(Var, Value)],
    prevReadEnv :: Maybe Env
}
    deriving Show

data Exception = ErrMsg String
    | ReturnExcept Env ExpStmt
    deriving Show

newEnv :: Env
newEnv = Env [] Nothing

-- newIOEnv :: IO Env
-- newIOEnv = do
--     var <- newIORef []
--     return $ Env var Nothing

varLookup :: Env -> Var -> Maybe Value
varLookup (Env store Nothing) var = lookup var store
varLookup (Env store (Just prev)) var = case lookup var store of
    (Just val) -> Just val
    Nothing -> varLookup prev var

varAssign :: Env -> (Var, Value) -> Env
varAssign (Env vStore prev) var = Env (var : vStore) prev

varReassign :: Env -> (Var, Value) -> Either Exception Env
varReassign (Env vStore prev) pair@(var, val) = case replaceIfExists var vStore val of
    (Just env) -> Right $ Env env prev
    Nothing -> case prev of
        (Just env) -> do
            env' <- varReassign env pair
            return $ Env vStore (Just env')
        Nothing -> Left $ ErrMsg $ "unbound variable `" ++ var ++ "`"

addScope :: Env -> Env -> Env
addScope prev (Env vStore prevEnv) = Env vStore (Just prev)

removeScope :: Env -> Maybe Env
removeScope (Env _ prev) = prev

replaceVarEnv :: Env -> [(Var, Value)] -> Env
replaceVarEnv (Env _ prev) vStore = Env vStore prev

-- replaceProcEnv :: Env -> [(Var, Proc)] -> Env
-- replaceProcEnv (Env vStore _ prev) pStore = Env vStore pStore prev

-- addProcDef :: Env -> (Var, Proc) -> Env
-- addProcDef (Env vStore pStore prev) var = Env vStore (var : pStore) prev

procCall _ _ _ = Left $ ErrMsg $ ""

-- procCall :: Env -> Var -> [ExpStmt] -> Either Exception (Env, Value)
-- procCall env@(Env _ store prev) var args = case lookup var store of
--     Nothing -> Left $ ErrMsg $ "no function with name " ++ var ++ " found"
--     (Just (xs, stmt)) -> 
--         if length xs /= length args then 
--             Left $ ErrMsg ("invalid number of arguments: " ++ show (length xs) ++ " expected but " ++ show (length args) ++ "were given")
--         else do
--             (env', values) <- vals env args
--             let env' = addScope env (replaceVarEnv newEnv (zip xs values))
--             (env'', val) <- runStmt env' stmt
--             Right (removedScope env'' val)
--         where
--             vals :: Env -> [ExpStmt] -> Either Exception (Env, [Value])
--             vals env [] = Right (env, [])
--             vals env (x:xs) = do
--                 (env', val) <- eval x env
--                 (env'', val') <- vals env' xs
--                 return (env'', val:val')

--             runStmt :: Env -> Stmt -> Either Exception (Env, Value)
--             runStmt env stmt = case exec stmt env of
--                 (Right env') -> Right (env', Null)
--                 (Left (ReturnExcept env' expStmt)) -> eval expStmt env'
--                 (Left (ErrMsg msg)) -> Left $ ErrMsg msg

--             removedScope :: Env -> Value -> (Env, Value)
--             removedScope env val = case removeScope env of
--                 (Just env') -> (env', val)
--                 Nothing -> (env, val)

replaceIfExists :: (Eq a) => a -> [(a, b)] -> b -> Maybe [(a, b)]
replaceIfExists _ [] _ = Nothing
replaceIfExists key (x:xs) value
    | key == fst x = Just $ (key, value) : xs
    | otherwise = (x :) <$> replaceIfExists key xs value

genericTypeException :: String -> String -> String
genericTypeException x y = 
    "cannot use value of type `" ++ x ++ "` where value of type `" ++ y ++ "` was expected"

add :: Value -> Value -> Either Exception Value
add (Num x) (Num y) = Right $ Num (x + y)
add (Float x) (Float y) = Right $ Float (x + y)
add x y =
    let v1 = valueTypeLookup x
        v2 = valueTypeLookup y
    in Left $ ErrMsg $ "cannot add values of type `" ++ v1 ++ "` and type `" ++ v2 ++ "`"

sub :: Value -> Value -> Either Exception Value
sub (Num x) (Num y) = Right $ Num (x - y)
sub (Float x) (Float y) = Right $ Float (x - y)
sub x y =
    let v1 = valueTypeLookup x
        v2 = valueTypeLookup y
    in Left $ ErrMsg $ "cannot subtract values of type `" ++ v1 ++ "` and type `" ++ v2 ++ "`"

mul :: Value -> Value -> Either Exception Value
mul (Num x) (Num y) = Right $ Num (x * y)
mul (Float x) (Float y) = Right $ Float (x * y)
mul x y =
    let v1 = valueTypeLookup x
        v2 = valueTypeLookup y
    in Left $ ErrMsg $ "cannot multiply values of type `" ++ v1 ++ "` and type `" ++ v2 ++ "`"

divide :: Value -> Value -> Either Exception Value
divide (Num x) (Num y) = Right $ Num (x `div` y)
divide (Float x) (Float y) = Right $ Float (x / y)
divide x y =
    let v1 = valueTypeLookup x
        v2 = valueTypeLookup y
    in Left $ ErrMsg $ "cannot divide values of type `" ++ v1 ++ "` and type `" ++ v2 ++ "`"

evalExp :: Exp -> Env -> Either Exception Value
evalExp (Lit n) _ = Right n
evalExp (Var x) env = case varLookup env x of
    Just v -> Right v
    Nothing -> Left $ ErrMsg $ "unbound variable `" ++ x ++ "`"
evalExp (Add x y) env = do
    v1 <- evalExp x env
    v2 <- evalExp y env
    add v1 v2
evalExp (Sub x y) env = do
    v1 <- evalExp x env
    v2 <- evalExp y env
    sub v1 v2
evalExp (Mul x y) env = do
    v1 <- evalExp x env
    v2 <- evalExp y env
    mul v1 v2
evalExp (Div x y) env = do
    v1 <- evalExp x env
    v2 <- evalExp y env
    divide v1 v2

eval :: ExpStmt -> Env -> Either Exception (Env, Value)
eval (Expr exp) env = do
    value <- evalExp exp env
    return (env, value)
eval (CallProc name args) env = procCall env name args

exec :: Stmt -> Env -> Either Exception Env
exec (VarAssign s exp) env = do
    (env', value) <- eval exp env
    return $ varAssign env' (s, value)
exec (VarReassign s exp) env = do
    (env', value) <- eval exp env
    varReassign env' (s, value)
exec (While exp stmt) env = do
    (env', value) <- eval exp env
    case value of
        (Bool True) -> exec (Seq [stmt, While exp stmt]) env'
        (Bool False) -> Right env'
        v -> Left $ ErrMsg $ genericTypeException (valueTypeLookup v) "Bool"
exec (Seq []) env = Right env
exec (Seq (x:xs)) env = exec x env >>= \env' -> exec (Seq xs) env'
exec (ProcDef s args stmt) env = 
    let procedure = Proc args stmt
    in Right $ varAssign env (s, procedure)
exec (CallExpStmt expStmt) env = case expStmt of
    (Expr exp) -> Left $ ErrMsg "unused result of expression"
    _ -> fst <$> eval expStmt env
exec (ReturnStmt expStmt) env = Left $ ReturnExcept env expStmt

runProgram :: Stmt -> Env -> Either String Env
runProgram stmt env = case exec stmt env of
    (Right env') -> Right env'
    (Left (ReturnExcept _ _)) -> Left "unexpected return statement not nested in function"
    (Left (ErrMsg msg)) -> Left msg

execNew :: Stmt -> Either String Env
execNew stmt = runProgram stmt newEnv

testAst :: Either String Env
testAst = execNew (
    Seq [
        VarAssign "x" (Expr $ Lit $ Num 5), 
        VarReassign "x" (Expr $ Lit $ Bool True),
        ProcDef "makeCounter" [] (Seq [
            VarAssign "i" (Expr $ Lit $ Num 0),
            ProcDef "count" [] (Seq [
                VarReassign "i" (Expr $ Add (Var "i") (Lit $ Num 1))
            ]),
            ReturnStmt (Expr $ Var "count")
        ])
        -- VarAssign "counter" (CallProc "makeCounter" [])
        -- ProcDef "function" [] (Seq [
        --     VarReassign "x" (Expr $ Lit $ Bool False),
        --     VarAssign "x" (Expr $ Lit $ Float 63.2),
        --     VarAssign "y" (Expr $ Lit $ Float 5.52),
        --     ReturnStmt (Expr $ Lit $ Float 4.3)
        -- ]),
        -- VarAssign "return" (CallProc "function" [])
        
        ]
    )
