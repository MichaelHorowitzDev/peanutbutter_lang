module Ast (
  Function (..),
  Value (..),
  getInt,
  getFloat,
  getFloating,
  getString,
  getBool,
  getArray,
  getFunc,
  isNull,
  valueTypeLookup,
  Exp (..),
  Stmt (..),
  Position(..),
  getExpPosition,
  getStmtPosition,
  getStmtName,
  Env (..),
  Val (..),
  NativeFunction (..),
  Exception (..),
  Var
) where

import Data.IORef
import System.IO.Unsafe
import Position
import InterpretError
import Control.Monad.Trans.Except
import qualified Data.Vector as V
import Control.Monad.Reader
import Control.Applicative
import qualified Data.Map.Strict as Map

data Exception = ErrMsg String
    | ReturnExcept Env Exp Position
    | InterpErr InterpretError
    deriving Show

type Var = String

data Val = Val { value :: Value, mutable :: Bool } deriving Show
data NativeFunction = NativeFunction {
    funcArity :: Int,
    runNativeFunc :: ReaderT [(Value, Position)] (ExceptT Exception IO) Value
    }

data Function = Function {
    funcParams :: [String],
    funcStmts :: [Stmt],
    funcEnv :: Env,
    runFunction :: ReaderT ([Value], Position) (ExceptT Exception IO) Value
}

data Env = Env {
    varEnv :: IORef (Map.Map Var Val),
    prevEnv :: Maybe Env
}
    deriving Show

instance (Show a) => Show (IORef a) where
    show a = show (unsafePerformIO (readIORef a))

data Stmt = VarAssign String Exp Position
    | VarReassign String Exp Position
    | LetAssign String Exp Position
    | While Exp [Stmt] Position
    | If [(Exp, [Stmt])] [Stmt] Position
    | FuncDef String [String] [Stmt] Position
    | DataDef String [String] [Stmt] Position
    | ReturnStmt Exp Position
    | CallExp Exp Position
    | Print Exp Position
    deriving Show

getStmtName :: Stmt -> String
getStmtName stmt = case stmt of
    VarAssign {} -> "var assign"
    LetAssign {} -> "let assign"
    VarReassign {} -> "var reassign"
    While {} -> "while"
    If {} -> "if"
    FuncDef {} -> "function definition"
    DataDef {} -> "class definition"
    ReturnStmt {} -> "return"
    CallExp {} -> "expression call"
    Print {} -> "print"


getStmtPosition :: Stmt -> Position
getStmtPosition (VarAssign _ _ pos) = pos
getStmtPosition (VarReassign _ _ pos) = pos
getStmtPosition (LetAssign _ _ pos) = pos
getStmtPosition (While _ _ pos) = pos
getStmtPosition (If _ _ pos) = pos
getStmtPosition (FuncDef _ _ _ pos) = pos
getStmtPosition (DataDef _ _ _ pos) = pos
getStmtPosition (ReturnStmt _ pos) = pos
getStmtPosition (CallExp _ pos) = pos
getStmtPosition (Print _ pos) = pos

data Value = Int Int
    | Float Float
    | String String
    | Bool Bool
    | Func Function
    | NativeFunc NativeFunction
    | Data [String] [Stmt] Env
    | DataInstance Env
    | Array (V.Vector Value) Int
    | List [Value]
    | Void
    | Null

getFloating :: Value -> Maybe Float
getFloating v = getFloat v <|> fromIntegral <$> getInt v

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

getArray :: Value -> Maybe (V.Vector Value, Int)
getArray (Array v n) = Just (v, n)
getArray _ = Nothing

getFunc :: Value -> Maybe Function
getFunc (Func function) = Just function
getFunc _ = Nothing

instance Show Value where
    show a = case a of
        (Int n) -> show n
        (Float f) -> show f
        (String s) -> show s
        (Bool b) -> show b
        (Func {}) -> "<func>"
        (NativeFunc {}) -> "<native_fn>"
        (Data {}) -> "<data>"
        (DataInstance {}) -> "<data_instance>"
        (Array vector n) -> "Array " ++ show (V.take n vector)
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
    | ArrayDef [Exp] Position
    | Subscript Exp Exp Position
    | Lit Value Position
    | Var String Position
    | Getter Exp String Position
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
getExpPosition (ArrayDef _ pos) = pos
getExpPosition (Subscript _ _ pos) = pos
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
    Data {} -> "Data"
    DataInstance {} -> "Data Instance"
    Void -> "Void"
    Array {} -> "Array"
    Null -> "Null"
    _ -> "Unknown Type"
