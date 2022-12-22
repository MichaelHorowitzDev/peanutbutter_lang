module Ast where

data Value = Num Int
    | Float Float
    | Str String
    | Bool Bool
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
    | CallProc String [Exp]
    deriving Show

data Stmt = VarAssign String Exp
    | VarReassign String Exp
    | While Exp Stmt
    | Seq [Stmt]
    | ProcDef String [String] Stmt
    deriving Show
