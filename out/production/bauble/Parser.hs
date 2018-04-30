module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tk

import Lexer
import Syntax

-- TODO try to change AssocLeft to AssocNone
binary s f = Ex.Infix (reservedOp s >> return (BinOperator f)) Ex.AssocLeft
unary s f = Ex.Prefix (reservedOp s >> return (UnOperator f))

numberOperatorsTable = [ [unary "-" Negate]
                       , [binary "*" Multiply, binary "/" Divide]
                       , [binary "+" Plus, binary "-" Minus]
                       , [binary "<" Less, binary "<=" LessOrEqual
                         ,binary "<" Greater, binary "<" GreaterOrEqual
                         ,binary "==" Equal, binary "!=" NotEqual] ]

boolOperatorsTable = [ [unary "not" Not]
                     , [binary "and" And, binary "or" Or] ]

operatorsTable = numberOperatorsTable ++ boolOperatorsTable

-- Values

integerValue :: Parser Expr
integerValue = do
  n <- integer
  return $ Value $ Integer n

doubleValue :: Parser Expr
doubleValue = do -- For parsec float is just alias of 'non-integer'
  n <- float
  return $ Value $ Double n

trueValue = do
  reserved "true"
  return $ Value $ Boolean True

falseValue = do
  reserved "false"
  return $ Value $ Boolean False

nullValue :: Parser Expr
nullValue = do
  reserved "null"
  return $ Value Null

-- Expressions

value :: Parser Expr
value =  try doubleValue
     <|> try integerValue
     <|> trueValue
     <|> falseValue
     <|> nullValue

variable = Variable <$> identifier

assign = do
  var <- identifier
  reservedOp "="
  someExpr <- expr
  return $ Assign var someExpr

function = do
  reserved "function"
  args <- parens $ commaSep identifier
  body <- braces $ endBy expr semi
  return $ Function args body

operatorExpr = Ex.buildExpressionParser operatorsTable expr

ifExpr = do
  reserved "if"
  conditional <- expr
  trueBranch <- braces $ endBy expr semi
  falseBranch <- braces $ endBy expr semi
  return $ If conditional trueBranch falseBranch

whileExpr = do
  reserved "while"
  conditional <- expr
  body <- braces $ endBy expr semi
  return $ While conditional body

returnExpr = do
  reserved "return"
  stmt <- expr
  return $ Return stmt

-- Function call

call :: Parser Expr
call = do
  callable <- parens $ function <|> variable
  args <- parens $ commaSep expr
  return $ Call callable args

-- General expression parser

expr :: Parser Expr
--expr = try value
--    <|> try assign
--    <|> try function
--    <|> try operatorExpr
--    <|> try ifExpr
--    <|> try whileExpr
--    <|> try returnExpr
--    <|> try call
expr = value

toplevel :: Parser [Expr]
toplevel = many $ do
    newExpr <- expr
    reservedOp ";"
    return newExpr

contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s

