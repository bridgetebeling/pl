module MiniRacketParser where

import Parser
import Expr
import Control.Applicative
import Error ( ErrorType ) 

parseBool :: Parser Bool
parseBool =
    (True <$ parseKeyword "true")
    <|> (False <$ parseKeyword "false")


-- parse binary bool operations
-- TODO: implement parsing bool operations which have 
--   two parameters, these are 'and' and 'or'
parseBoolOp :: Parser BoolOp
parseBoolOp = (And <$ parseKeyword "and")
          <|> (Or <$ parseKeyword "or")

    

-- parse math operations and return the MathOp
-- TODO: Add the other math operations: *, div, mod
parseMathOp :: Parser MathOp
parseMathOp =
    (Add <$ symbol "+")
    <|> (Sub <$ symbol "-")
    <|> (Mul <$ symbol "*")
    <|> (Div <$ parseKeyword "div")
    <|> (Mod <$ parseKeyword "mod")

    

-- parse the comparison operations and return the corresponding  CompOp
-- TODO: add the comparison operators: equal?, < 
parseCompOp :: Parser CompOp
parseCompOp =
    (Eq <$ parseKeyword "equal?")
    <|> (Lt <$ symbol "<")


-- a literal in MiniRacket is true, false, or a number
-- TODO: parse the literals: true, false, and numbers
literal :: Parser Value
literal = (BoolValue True <$ parseKeyword "true")
      <|> (BoolValue False <$ parseKeyword "false")
      <|> (IntValue <$> natural)


-- parse a literal expression, which is just a literal
literalExpr :: Parser Expr
literalExpr = do
    LiteralExpr <$> literal


keywordList :: [String]
keywordList = ["false", "true", "not", "and", "or", "div", "mod", "equal?"]

-- try to parse a keyword, otherwise it is a variable, this can be
-- used to check if the identifier we see (i.e., variable name) is
-- actually a keyword, which is not legal
parseKeyword :: String -> Parser String
parseKeyword kw = do
    _ <- token (string kw)
    return kw


-- TODO: parse not expressions, note that "not" is a keyword,
-- (HINT: you should use parseKeyword)
notExpr :: Parser Expr
notExpr = do
  parseKeyword "not"
  expr <- parseExpr
  return $ NotExpr expr


-- TODO: parse boolean expressions
-- a bool expression is the operator followed by one or more expressions
boolExpr :: Parser Expr
boolExpr = do
  op <- parseBoolOp
  exprs <- oneOrMore parseExpr
  return $ BoolExpr op exprs


-- TODO: parse maths expressions
-- a math expression is the operator followed by one or more expressions
mathExpr :: Parser Expr
mathExpr = do
  op <- parseMathOp
  exprs <- oneOrMore parseExpr
  return $ MathExpr op exprs


-- a comparison expression is the comparison operator
--   followed by two expressions
compExpr :: Parser Expr
compExpr = CompExpr <$> parseCompOp <*> parseExpr <*> parseExpr

pairExpr :: Parser Expr
pairExpr = do
    expr1 <- parseExpr
    symbol "."
    PairExpr expr1 <$> parseExpr

-- note that this is syntactic sugar, cons is just replaced by a 
--    PairExpr abstract syntax tree 
consExpr :: Parser Expr 
consExpr = do 
    symbol "cons"
    expr1 <- parseExpr 
    PairExpr expr1 <$> parseExpr 

parseParens :: Parser Expr -> Parser Expr
parseParens p = do
    symbol "("
    e <- p
    symbol ")"
    return e

-- the main parsing function which alternates between all
-- the options you have for possible expressions
-- TODO: Add new expression types here
parseExpr :: Parser Expr
parseExpr =
    parseParens notExpr
    <|> parseParens boolExpr
    <|> parseParens compExpr    
    <|> parseParens mathExpr
    <|> parseParens pairExpr
    <|> parseParens consExpr
    <|> literalExpr



-- a helper function for testing parsing
--   To use simply type:
--      parseString "5" 
--   this will use the parseExpr Parser to parse the contents of str
parseString :: String -> Either ErrorType (Expr, String) 
parseString str = do 
    parse parseExpr str
