module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tk

import Lexer
import Syntax

-- TODO try to change AssocLeft to AssocNone
binary s f assoc = Ex.Infix (reservedOp s >> return (BinOperator f)) assoc
unary s f = Ex.Prefix (reservedOp s >> return (UnOperator f))

numberOperatorsTable = [ [unary "-" Negate]
                       , [binary "*" Multiply Ex.AssocLeft, binary "/" Divide Ex.AssocLeft]
                       , [binary "+" Plus Ex.AssocLeft, binary "-" Minus Ex.AssocLeft]
                       , [binary "<" Less Ex.AssocNone, binary "<=" LessOrEqual Ex.AssocNone
                         ,binary "<" Greater Ex.AssocNone, binary "<" GreaterOrEqual Ex.AssocNone
                         ,binary "==" Equal Ex.AssocNone, binary "!=" NotEqual Ex.AssocNone] ]

boolOperatorsTable = [ [unary "not" Not]
                     , [binary "and" And Ex.AssocLeft, binary "or" Or Ex.AssocLeft] ]

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
     <|> try trueValue
     <|> try falseValue
     <|> try nullValue

variable = Variable <$> identifier

assign = do
  var <- identifier
  reservedOp "="
  someExpr <- simpleExpr
  return $ Assign var someExpr

returnExpr = do
  reserved "return"
  stmt <- simpleExpr
  reservedOp ";"
  return $ Return stmt

function = do
  reserved "function"
  args <- parens $ commaSep identifier
  body <- braces $ many (try anyExpr <|> try returnExpr)
  return $ Function args body

operatorExpr = Ex.buildExpressionParser operatorsTable simpleExpr'

ifExpr = do
  reserved "if"
  conditional <- parens simpleExpr
  trueBranch <- braces toplevel
  reserved "else"
  falseBranch <- braces toplevel
  return $ If conditional trueBranch falseBranch

whileExpr = do
  reserved "while"
  conditional <- anyExpr
  body <- braces toplevel
  return $ While conditional body

-- Function call

call :: Parser Expr
call = do
  callable <- try variable <|> try (parens function)
  args <- parens $ commaSep simpleExpr
  return $ Call callable args


-- Helper parser for operators table
simpleExpr' =  try function
           <|> try call
           <|> try variable
           <|> try value
           <|> try (parens operatorExpr)

-- Expressions that can be returned
simpleExpr =  try operatorExpr
          <|> try simpleExpr'
          <|> try (parens simpleExpr')

-- Flow expressions
flowExpr = try assign
       <|> try whileExpr
       <|> try ifExpr

anyExpr :: Parser Expr
anyExpr = do
  newExpr <- try flowExpr <|>
             try operatorExpr <|>
             try simpleExpr
  case newExpr of
       Function _ _ -> return newExpr
       While _ _    -> return newExpr
       If _ _ _     -> return newExpr
       _            -> do reservedOp ";"
                          return newExpr

toplevel :: Parser [Expr]
toplevel = many anyExpr


contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s

