module Ast (
  Function (..),
  Value (..),
  valueTypeLookup,
  Exp (..),
  ExpStmt (..),
  Stmt (..),
  Env (..),
  Var
) where
  
  
type Var = String

data Env = Env {
    varEnv :: [(Var, Value)],
    prevEnv :: Maybe Env
}
    deriving Show
    
--data FuncEnv = FuncEnv {
--  funcStore :: [(Var, Value)],
--  parentFunc :: Maybe Function
--}
    
data Function = Function {
  funcParams :: [String],
  funcStmts :: Stmt,
  funcEnv :: Env
 }
 
data ExpStmt = CallFunc String [ExpStmt]
    | Expr Exp
    deriving Show

data Stmt = VarAssign String ExpStmt
    | VarReassign String ExpStmt
    | While ExpStmt Stmt
    | Seq [Stmt]
    | FuncDef String [String] Stmt
    | ReturnStmt ExpStmt
    | CallExpStmt ExpStmt
    | Print ExpStmt
    deriving Show
    
data Value = Float Float
    | String String
--    | Var String
    | Bool Bool
    | Func [String] Stmt Env
    | Null
    deriving Show

data Exp = Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | Div Exp Exp
    | Equal Exp Exp
    | NotEqual Exp Exp
    | Greater Exp Exp
    | GreaterEqual Exp Exp
    | Less Exp Exp
    | LessEqual Exp Exp
    | Negate Exp
    | Bang Exp
    | Lit Value
    | Var String
--    | Float Float
--    | String String
--    | Identifier String
--    | Bool Bool
--    | Null
--    | Func [String] Stmt Env
    deriving Show

--data Value = Num Int
--    | Float Float
--    | Str String
--    | Bool Bool
--    | Func [String] Stmt Env
--    | Null
--    deriving Show

valueTypeLookup :: Value -> String
valueTypeLookup v = case v of
--    Num {} -> "Int"
    Float {} -> "Float"
    String {} -> "String"
    Bool {} -> "Bool"
    _ -> "Unknown type"

--data Exp = Lit Value
--    | Var String
--    | Add Exp Exp
--    | Sub Exp Exp
--    | Mul Exp Exp
--    | Div Exp Exp
--    deriving Show

--data ExpStmt = CallFunc String [ExpStmt]
--    | Expr Exp
--    deriving Show

--data Stmt = VarAssign String ExpStmt
--    | VarReassign String ExpStmt
--    | While ExpStmt Stmt
--    | Seq [Stmt]
--    | FuncDef String [String] Stmt
--    | ReturnStmt ExpStmt
--    | CallExpStmt ExpStmt
--    | Type String
--    deriving Show
