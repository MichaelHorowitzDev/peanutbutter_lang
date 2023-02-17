module Ast (
  Function (..),
  Value (..),
  isNull,
  valueTypeLookup,
  Exp (..),
  Stmt (..),
  Position(..),
  getExpPosition,
  Env (..),
  Val (..),
  Var
) where
  
import Data.IORef
import System.IO.Unsafe
  
type Var = String

data Val = Val { value :: Value, mutable :: Bool } deriving Show

data Env = Env {
    varEnv :: IORef [(Var, Val)],
    prevEnv :: Maybe Env
}
    deriving Show

instance (Show a) => Show (IORef a) where
    show a = show (unsafePerformIO (readIORef a))
    
data Function = Function {
  funcParams :: [String],
  funcStmts :: Stmt,
  funcEnv :: Env
  }

data Position = Position { posOffset :: Int, posLength :: Int } deriving (Eq, Ord, Show)

data Stmt = VarAssign String Exp Position
    | VarReassign String Exp Position
    | LetAssign String Exp Position
    | While Exp Stmt Position
    | If [(Exp, Stmt)] Stmt Position
    | Seq [Stmt]
    | FuncDef String [String] Stmt Position
    | ReturnStmt Exp Position
    | CallExp Exp Position
    | Print Exp Position
    deriving Show
    
data Value = Int Int
    | Float Float
    | String String
    | Bool Bool
    | Func [String] Stmt Env
    | Null
    deriving Show

isNull :: Value -> Bool
isNull Null = True
isNull _ = False

data Exp = Add Exp Exp Position
    | Sub Exp Exp Position
    | Mul Exp Exp Position
    | Div Exp Exp Position
    | Equal Exp Exp Position
    | NotEqual Exp Exp Position
    | Greater Exp Exp Position
    | GreaterEqual Exp Exp Position
    | Less Exp Exp Position
    | LessEqual Exp Exp Position
    | Negate Exp Position
    | Bang Exp Position
    | CallFunc Exp [Exp] Position
    | Lambda [String] Exp Position
    | Lit Value Position
    | Var String Position
    deriving Show

getExpPosition :: Exp -> Position
getExpPosition (Add _ _ pos) = pos
getExpPosition (Sub _ _ pos) = pos
getExpPosition (Mul _ _ pos) = pos
getExpPosition (Div _ _ pos) = pos
getExpPosition (Equal _ _ pos) = pos
getExpPosition (NotEqual _ _ pos) = pos
getExpPosition (Greater _ _ pos) = pos
getExpPosition (GreaterEqual _ _ pos) = pos
getExpPosition (Less _ _ pos) = pos
getExpPosition (LessEqual _ _ pos) = pos
getExpPosition (Negate _ pos) = pos
getExpPosition (Bang _ pos) = pos
getExpPosition (CallFunc _ _ pos) = pos
getExpPosition (Lambda _ _ pos) = pos
getExpPosition (Lit _ pos) = pos
getExpPosition (Var _ pos) = pos

valueTypeLookup :: Value -> String
valueTypeLookup v = case v of
    Int {} -> "Int"
    Float {} -> "Float"
    String {} -> "String"
    Bool {} -> "Bool"
    _ -> "Unknown type"
