module Interpret where

import Ast

type Var = String
type Store = [(Var, Value)]
type Proc = ([String], Stmt)

type Error = String

type Prog = Stmt

data Env = Env { 
    varEnv :: [(Var, Value)], 
    procEnv :: [(Var, Proc)], 
    prevEnv :: Maybe Env 
    }
    deriving Show

newEnv :: Env
newEnv = Env [] [] Nothing

varLookup :: Env -> Var -> Maybe Value
varLookup (Env store _ Nothing) var = lookup var store
varLookup (Env store _ (Just prev)) var = case lookup var store of
    (Just val) -> Just val
    Nothing -> varLookup prev var

varAssign :: Env -> (Var, Value) -> Env
varAssign (Env vStore pStore prev) var = Env (var : vStore) pStore prev

varReassign :: Env -> (Var, Value) -> Either Error Env
varReassign (Env vStore pStore prev) pair@(var, val) = case replaceIfExists var vStore val of
    (Just env) -> Right $ Env env pStore prev
    Nothing -> case prev of
        (Just env) -> do
            env' <- varReassign env pair
            return $ Env vStore pStore (Just env')
        Nothing -> Left $ "unbound variable `" ++ var ++ "`"

addScope :: Env -> Env -> Env
addScope prev (Env vStore pStore prevEnv) = Env vStore pStore (Just prev)

removeScope :: Env -> Maybe Env
removeScope (Env vStore pStore prev) = prev

replaceVarEnv :: Env -> [(Var, Value)] -> Env
replaceVarEnv (Env _ pStore prev) vStore = Env vStore pStore prev

replaceProcEnv :: Env -> [(Var, Proc)] -> Env
replaceProcEnv (Env vStore _ prev) pStore = Env vStore pStore prev

addProcDef :: Env -> (Var, Proc) -> Env
addProcDef (Env vStore pStore prev) var = Env vStore (var : pStore) prev

procCall :: Env -> Var -> [ExpStmt] -> Either Error (Env, Value)
procCall env@(Env _ store prev) var args = case lookup var store of
    Nothing -> Left $ "no function with name " ++ var ++ " found"
    (Just (xs, stmt)) -> 
        if length xs /= length args then 
            Left ("invalid number of arguments: " ++ show (length xs) ++ " expected but " ++ show (length args) ++ "were given")
        else do
            (env', values) <- vals env args
            let env' = addScope env (replaceVarEnv newEnv (zip xs values))
            env'' <- exec stmt env'
            case removeScope env'' of
                (Just env''') -> Right (env''', Null)
                Nothing -> Right (env'', Null)
        where
            vals :: Env -> [ExpStmt] -> Either Error (Env, [Value])
            vals env [] = Right (env, [])
            vals env (x:xs) = do
                (env', val) <- eval x env
                (env'', val') <- vals env' xs
                return (env'', val:val')

replaceIfExists :: (Eq a) => a -> [(a, b)] -> b -> Maybe [(a, b)]
replaceIfExists _ [] _ = Nothing
replaceIfExists key (x:xs) value
    | key == fst x = Just $ (key, value) : xs
    | otherwise = (x :) <$> replaceIfExists key xs value

genericTypeError :: String -> String -> String
genericTypeError x y = 
    "cannot use value of type `" ++ x ++ "` where value of type `" ++ y ++ "` was expected"

add :: Value -> Value -> Either Error Value
add (Num x) (Num y) = Right $ Num (x + y)
add (Float x) (Float y) = Right $ Float (x + y)
add x y =
    let v1 = valueTypeLookup x
        v2 = valueTypeLookup y
    in Left $ "cannot add values of type `" ++ v1 ++ "` and type `" ++ v2 ++ "`"

sub :: Value -> Value -> Either Error Value
sub (Num x) (Num y) = Right $ Num (x - y)
sub (Float x) (Float y) = Right $ Float (x - y)
sub x y =
    let v1 = valueTypeLookup x
        v2 = valueTypeLookup y
    in Left $ "cannot subtract values of type `" ++ v1 ++ "` and type `" ++ v2 ++ "`"

mul :: Value -> Value -> Either Error Value
mul (Num x) (Num y) = Right $ Num (x * y)
mul (Float x) (Float y) = Right $ Float (x * y)
mul x y =
    let v1 = valueTypeLookup x
        v2 = valueTypeLookup y
    in Left $ "cannot multiply values of type `" ++ v1 ++ "` and type `" ++ v2 ++ "`"

divide :: Value -> Value -> Either Error Value
divide (Num x) (Num y) = Right $ Num (x `div` y)
divide (Float x) (Float y) = Right $ Float (x / y)
divide x y =
    let v1 = valueTypeLookup x
        v2 = valueTypeLookup y
    in Left $ "cannot divide values of type `" ++ v1 ++ "` and type `" ++ v2 ++ "`"

evalExp :: Exp -> Env -> Either Error Value
evalExp (Lit n) _ = Right n
evalExp (Var x) env = case varLookup env x of
    Just v -> Right v
    Nothing -> Left $ "unbound variable `" ++ x ++ "`"
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

eval :: ExpStmt -> Env -> Either Error (Env, Value)
eval (Expr exp) env = do
    value <- evalExp exp env
    return (env, value)
eval (CallProc name args) env = procCall env name args

exec :: Stmt -> Env -> Either Error Env
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
        v -> Left $ genericTypeError (valueTypeLookup v) "Bool"
exec (Seq []) env = Right env
exec (Seq (x:xs)) env = exec x env >>= \env' -> exec (Seq xs) env'
exec (ProcDef s args stmt) env = 
    let procedure = (args, stmt)
    in Right $ addProcDef env (s, procedure)
exec (CallExpStmt expStmt) env = case expStmt of
    (Expr exp) -> Left "unused result of expression"
    _ -> fst <$> eval expStmt env

execNew :: Stmt -> Either Error Env
execNew stmt = exec stmt newEnv

testAst :: Either Error Env
testAst = execNew (
    Seq [
        VarAssign "x" (Expr $ Lit $ Num 5), 
        VarReassign "x" (Expr $ Lit $ Bool True),
        ProcDef "function" [] (Seq [
            VarAssign "x" (Expr $ Lit $ Float 63.2),
            VarAssign "y" (Expr $ Lit $ Float 5.52)
        ]),
        CallExpStmt (CallProc "function" [])
        ]
    )
