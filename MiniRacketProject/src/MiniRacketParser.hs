module MiniRacketParser where

import Parser
import Expr
import Control.Applicative
import Parser (failParse)
import Error (ErrorType)


parseBool :: Parser Bool
parseBool =
    (True <$ parseKeyword "true")
    <|> (False <$ parseKeyword "false")

-- parse binary bool operations
parseBoolOp :: Parser BoolOp
parseBoolOp = (And <$ parseKeyword "and")
          <|> (Or <$ parseKeyword "or")

-- parse math operations and return the MathOp
parseMathOp :: Parser MathOp
parseMathOp =
    (Add <$ symbol "+")
    <|> (Sub <$ symbol "-")
    <|> (Mul <$ symbol "*")
    <|> (Div <$ parseKeyword "div")
    <|> (Mod <$ parseKeyword "mod")

-- parse the comparison operations and return the corresponding CompOp
parseCompOp :: Parser CompOp
parseCompOp =
    (Eq <$ parseKeyword "equal?")
    <|> (Lt <$ symbol "<")

-- a literal in MiniRacket is true, false, or a number
literal :: Parser Value
literal = (BoolValue True <$ parseKeyword "true")
      <|> (BoolValue False <$ parseKeyword "false")
      <|> (IntValue <$> natural)

-- parse a literal expression, which is just a literal
literalExpr :: Parser Expr
literalExpr = LiteralExpr <$> literal

keywordList :: [String]
keywordList = ["false", "true", "not", "and", "or", "div", "mod", "equal?"]

-- try to parse a keyword, otherwise it is a variable, this can be
-- used to check if the identifier we see is actually a keyword
parseKeyword :: String -> Parser String
parseKeyword kw = token (string kw)

notExpr :: Parser Expr
notExpr = do
  parseKeyword "not"
  expr <- parseExpr
  return $ NotExpr expr

boolExpr :: Parser Expr
boolExpr = do
  op <- parseBoolOp
  exprs <- oneOrMore parseExpr
  return $ BoolExpr op exprs

mathExpr :: Parser Expr
mathExpr = do
  op <- parseMathOp
  exprs <- oneOrMore parseExpr
  return $ MathExpr op exprs

compExpr :: Parser Expr
compExpr = CompExpr <$> parseCompOp <*> parseExpr <*> parseExpr

pairExpr :: Parser Expr
pairExpr = do
  expr1 <- parseExpr
  symbol "."
  PairExpr expr1 <$> parseExpr

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

parseExpr :: Parser Expr
parseExpr =
    parseParens ifExpr
    <|> parseParens letExpr
    <|> parseParens notExpr
    <|> parseParens boolExpr
    <|> parseParens mathExpr
    <|> parseParens compExpr
    <|> parseParens pairExpr
    <|> parseParens consExpr
    <|> parseAtom

parseAtom :: Parser Expr
parseAtom = literalExpr <|> negateAtom <|> varExpr

negateAtom :: Parser Expr
negateAtom = do
  symbol "-"
  name <- identifier
  if name `elem` keywordList then failParse "Invalid identifier after -" else
    return $ MathExpr Sub [LiteralExpr (IntValue 0), VarExpr name]

varExpr :: Parser Expr
varExpr = do
  name <- identifier
  if name `elem` keywordList then failParse "keyword is not a variable" else
    return $ VarExpr name

ifExpr :: Parser Expr
ifExpr = do
  parseKeyword "if"
  cond <- parseExpr
  thenExpr <- parseExpr
  elseExpr <- parseExpr
  return $ IfExpr cond thenExpr elseExpr

letExpr :: Parser Expr
letExpr = do
  parseKeyword "let"
  symbol "("
  name <- identifier
  boundExpr <- parseExpr
  symbol ")"
  body <- parseExpr
  return $ LetExpr name boundExpr body

-- helper for testing parsing
parseString :: String -> Either ErrorType (Expr, String) 
parseString str = parse parseExpr str
