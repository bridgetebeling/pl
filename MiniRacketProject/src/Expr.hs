-- File: src/Expr.hs
module Expr where

-- define the operator types
data BoolOp = And | Or deriving (Show, Eq)

data MathOp = Add | Sub | Mul | Div | Mod deriving (Show, Eq)

data CompOp = Eq | Lt deriving (Show, Eq)

-- define the expression types
data Expr = 
      BoolExpr BoolOp [Expr]
    | NotExpr Expr
    | MathExpr MathOp [Expr] 
    | CompExpr CompOp Expr Expr
    | VarExpr String
    | LiteralExpr Value 
    | IfExpr Expr Expr Expr
    | LetExpr String Expr Expr
    | PairExpr Expr Expr 
    | EmptyExpr 
    deriving (Show, Eq)

-- define the type for values
data Value = 
      IntValue Integer
    | BoolValue Bool 
    | PairValue (Value, Value)
    | ClosureValue String String Expr [(String, Value)] -- inlined ValueEnv
    deriving (Show, Eq)
