module Ast (
  Function (..),
  Value (..),
  getInt,
  getFloat,
  getString,
  getBool,
  getArray,
  isNull,
  valueTypeLookup,
  Exp (..),
  Stmt (..),
  Position(..),
  getExpPosition,
  Env (..),
  Val (..),
  Exception (..),
  Var
) where
  
import Data.IORef
import System.IO.Unsafe
import Position
import InterpretError
import Control.Monad.Trans.Except
import qualified Data.Vector as V

data Exception = ErrMsg String
    | ReturnExcept Env Exp
    | InterpErr InterpretError
    deriving Show
  
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
    | NativeFunc Int ([(Value, Position)] -> ExceptT Exception IO Value)
    | Array (V.Vector Exp)
    | Void
    | Null

getInt :: Value -> Maybe Int
getInt (Int n) = Just n
getInt _ = Nothing

getFloat :: Value -> Maybe Float
getFloat (Float f) = Just f
getFloat _ = Nothing

getString :: Value -> Maybe String
getString (String s) = Just s
getString _ = Nothing

getBool :: Value -> Maybe Bool
getBool (Bool b) = Just b
getBool _ = Nothing

getArray :: Value -> Maybe (V.Vector Exp)
getArray (Array v) = Just v
getArray _ = Nothing

instance Show Value where
    show a = case a of
        (Int n) -> show n
        (Float f) -> show f
        (String s) -> show s
        (Bool b) -> show b
        (Func {}) -> "<func>"
        (NativeFunc {}) -> "<native_fn>"
        (Array vector) -> "Array " ++ show vector
        Void -> "Void"
        Null -> "Null"
        

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
    | Subscript Exp Exp Position
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
    Func {} -> "Function"
    NativeFunc {} -> "Function"
    Void -> "Void"
    Array {} -> "Array"
    Null -> "Null"
    _ -> "Unknown type"
