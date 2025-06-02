-- File: src/Eval.hs
module Eval (
    evalString
) where

import Expr
import qualified Environment as Env
import Error
import MiniRacketParser (parseExpr)
import Parser (parse)



-- Evaluate a string input into a Value, using the top-level parser and an empty environment
evalString :: String -> Either ErrorType Value
evalString input = do
    (expr, _) <- parse parseExpr input
    evalExpr expr Env.emptyEnv

-- Top-level expression evaluator
evalExpr :: Expr -> Env.Env -> Either ErrorType Value
evalExpr expr env = case expr of
    LiteralExpr v -> Right v

    VarExpr name -> case Env.lookup name env of
        Just val -> Right val
        Nothing -> Left (NoSymbol name)

    IfExpr cond tr fl -> do
        condVal <- evalExpr cond env
        case condVal of
            BoolValue True -> evalExpr tr env
            BoolValue False -> evalExpr fl env
            _ -> Left (TypeError "Expected boolean in if condition")

    LetExpr name valExpr body -> do
        val <- evalExpr valExpr env
        let newEnv = Env.bindName name val env
        evalExpr body newEnv

    NotExpr e -> do
        val <- evalExpr e env
        case val of
            BoolValue b -> Right $ BoolValue (not b)
            _ -> Left (TypeError "Expected boolean in not expression")

    BoolExpr op [e1, e2] -> do
        v1 <- evalExpr e1 env
        v2 <- evalExpr e2 env
        case (v1, v2) of
            (BoolValue b1, BoolValue b2) -> case op of
                And -> Right $ BoolValue (b1 && b2)
                Or -> Right $ BoolValue (b1 || b2)
            _ -> Left (TypeError "Expected booleans in boolean expression")

    MathExpr op exprs -> do
        vals <- mapM (`evalExpr` env) exprs
        nums <- mapM unpackInt vals
        case op of
            Add -> Right $ IntValue (sum nums)
            Sub -> case nums of
                [x, y] -> Right $ IntValue (x - y)
                _ -> Left (EvalError "Sub expects two arguments")
            Mul -> Right $ IntValue (product nums)
            Div -> case nums of
                [x, y] -> if y == 0 then Left (EvalError "Division by zero") else Right $ IntValue (div x y)
                _ -> Left (EvalError "Div expects two arguments")
            Mod -> case nums of
                [x, y] -> Right $ IntValue (mod x y)
                _ -> Left (EvalError "Mod expects two arguments")

    CompExpr op e1 e2 -> do
        v1 <- evalExpr e1 env
        v2 <- evalExpr e2 env
        case op of
            Lt -> do
                x <- unpackInt v1
                y <- unpackInt v2
                Right $ BoolValue (x < y)
            Eq -> Right $ BoolValue (v1 == v2)

    PairExpr e1 e2 -> do
        v1 <- evalExpr e1 env
        v2 <- evalExpr e2 env
        Right $ PairValue (v1, v2)

-- Utility: Unpack Value into Integer
unpackInt :: Value -> Either ErrorType Integer
unpackInt (IntValue n) = Right n
unpackInt _ = Left (TypeError "Expected integer")
