module Ast (
  Function (..),
  Value (..),
  valueTypeLookup,
  Exp (..),
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
    
data Function = Function {
  funcParams :: [String],
  funcStmts :: Stmt,
  funcEnv :: Env
 }

data Stmt = VarAssign String Exp
    | VarReassign String Exp
    | While Exp Stmt
    | If [(Exp, Stmt)] Stmt
    | Seq [Stmt]
    | FuncDef String [String] Stmt
    | ReturnStmt Exp
    | CallExp Exp
    | Print Exp
    deriving Show
    
data Value = Int Int
    | Float Float
    | String String
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
    | CallFunc Exp [Exp]
    | Lambda [String] Exp
    | Lit Value
    | Var String
    deriving Show

valueTypeLookup :: Value -> String
valueTypeLookup v = case v of
    Int {} -> "Int"
    Float {} -> "Float"
    String {} -> "String"
    Bool {} -> "Bool"
    _ -> "Unknown type"
