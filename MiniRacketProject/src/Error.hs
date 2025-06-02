-- File: src/Error.hs
module Error where

data ErrorType =
      EvalError String
    | ParseError String
    | TypeError String
    | NoSymbol String
    deriving (Eq)

instance Show ErrorType where
    show (EvalError s) = "Evaluation error: " ++ s
    show (ParseError s) = "Parse error: " ++ s
    show (TypeError s) = "Type error: " ++ s
    show (NoSymbol s) = "Unbound variable: " ++ s
