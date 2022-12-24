module Ast where

data Value = Num Int
    | Float Float
    | Str String
    | Bool Bool
    | Proc [String] Stmt
    | Null
    deriving Show

valueTypeLookup :: Value -> String
valueTypeLookup v = case v of
    Num {} -> "Int"
    Float {} -> "Float"
    Str {} -> "String"
    Bool {} -> "Bool"

data Exp = Lit Value
    | Var String
    | Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | Div Exp Exp
    deriving Show

data ExpStmt = CallProc String [Stmt]
    | Expr Exp
    deriving Show

data Stmt = VarAssign String ExpStmt
    | VarReassign String ExpStmt
    | While ExpStmt Stmt
    | Seq [Stmt]
    | ProcDef String [String] Stmt
    | Return ExpStmt
    | CallExpStmt ExpStmt
    deriving Show
