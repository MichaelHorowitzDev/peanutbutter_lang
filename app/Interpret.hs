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

eval :: Exp -> Env -> Either Error Value
eval (Lit n) _ = Right n
eval (Var x) env = case varLookup env x of
    Just v -> Right v
    Nothing -> Left $ "unbound variable `" ++ x ++ "`"
eval (Add x y) env = do
    v1 <- eval x env
    v2 <- eval y env
    add v1 v2
eval (Sub x y) env = do
    v1 <- eval x env
    v2 <- eval y env
    sub v1 v2
eval (Mul x y) env = do
    v1 <- eval x env
    v2 <- eval y env
    mul v1 v2
eval (Div x y) env = do
    v1 <- eval x env
    v2 <- eval y env
    divide v1 v2

exec :: Stmt -> Env -> Either Error Env
exec (VarAssign s exp) env = do
    value <- eval exp env 
    return $ varAssign env (s, value)
exec (VarReassign s exp) env = do
    value <- eval exp env
    varReassign env (s, value)
exec (While exp stmt) env = do
    case eval exp env of
        (Right (Bool True)) -> exec (Seq [stmt, While exp stmt]) env
        (Right (Bool False)) -> Right env
        (Right v) -> Left $ genericTypeError (valueTypeLookup v) "Bool"
        (Left err) -> Left err
exec (Seq []) env = Right env
exec (Seq (x:xs)) env = exec x env >>= \env' -> exec (Seq xs) env'

testAst :: Either Error Env
testAst = exec (Seq [VarAssign "y" (Lit $ Num 5), VarReassign "y" (Lit $ Bool True)]) newEnv
