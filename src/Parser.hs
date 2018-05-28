{-# LANGUAGE ViewPatterns #-}

module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tk

import Lexer
import Syntax

-- ######################
-- ### VALUE PARSERS ###
-- ######################


integerValue = Integer <$> integer
doubleValue = Double <$> float

falseValue = reserved "false" >> return (Boolean False)
trueValue = reserved "true" >> return (Boolean True)
            
nullValue = reserved "null" >> return Null

stringValue =  String <$> stringLiteral

-- Builtin functions
printValue = reserved "print" >> return (BuiltinFunction Print)

isNullValue = reserved "isnull" >> return (BuiltinFunction IsNull)

simpleValue :: Parser Value
simpleValue =  try doubleValue
           <|> try integerValue
           <|> try trueValue
           <|> try falseValue
           <|> try nullValue
           <|> try stringValue
           <|> try printValue
           <|> try isNullValue

arrayValue = do
  exprs <- (brackets $ commaSep expression)
  return (Array exprs)

valueExpr :: Parser Expression
valueExpr = do
  value <- try simpleValue <|> try arrayValue
  return (Value value)

-- ##########################
-- ### EXPRESSION PARSERS ###
-- ##########################

-- OPERATORS TABLE

binary s f assoc = Ex.Infix (reservedOp s >> return (BinaryOp f)) assoc
unary s f = Ex.Prefix (reservedOp s >> return (UnaryOp f))

numberOperatorsTable = [ [ unary  "-"  Negate]
                       , [ binary "*"  Multiply       Ex.AssocLeft
                         , binary "/"  Divide         Ex.AssocLeft]
                       , [ binary "+"  Plus           Ex.AssocLeft
                         , binary "-"  Minus          Ex.AssocLeft]
                       , [ binary "<"  Less           Ex.AssocNone
                         , binary "<=" LessOrEqual    Ex.AssocNone
                         , binary "<"  Greater        Ex.AssocNone
                         , binary "<"  GreaterOrEqual Ex.AssocNone
                         , binary "==" Equal          Ex.AssocNone
                         , binary "!=" NotEqual       Ex.AssocNone] ]

boolOperatorsTable = [ [ unary "not"  Not]
                     , [ binary "and" And Ex.AssocNone
                       , binary "or"  Or  Ex.AssocNone] ]

operatorsTable = numberOperatorsTable ++ boolOperatorsTable

operatorExpr = Ex.buildExpressionParser operatorsTable expression

-- Helper function, that detectes call (parens and comma-separated values)
checkNextCalls call = do
  nextArgs <- optionMaybe (parens $ commaSep expression)
  case nextArgs of
    Nothing   -> return $ call
    Just args -> checkNextCalls (Call call args)

lambdaExprCall = (parens functionExpr) >>= checkNextCalls

variableExpr = Variable <$> identifier

functionExpr = do
  reserved "function"
  name <- optionMaybe identifier
  args <- parens $ commaSep identifier
  body <- (statementsBlock True False)

  -- `function () {} ()` is not allowed
  -- `(function () {}) ()` should be used for lambda call
  notFollowedBy $ (parens $ commaSep identifier)
  return $ Function name args body

lambdaCallExpr = (parens functionExpr) >>= checkNextCalls

expression' :: Parser Expression
expression' =  try valueExpr
           <|> try operatorExpr
           <|> try variableExpr
           <|> try lambdaCallExpr
           <|> try functionExpr
           <|> try (parens expression')
           

expression :: Parser Expression
expression = expression' >>= checkNextCalls


-- #########################
-- ### STATEMENT PARSERS ###
-- #########################

assignStmt = do
  var <- identifier
  reservedOp "="
  someExpr <- expression
  return $ Assign var someExpr

ifStmt wFunc wWhile = do
  reserved "if"
  conditional <- parens expression
  trueBranch <- (statementsBlock wFunc wWhile)
  let elseParser = reserved "else" >> (statementsBlock wFunc wWhile)
  falseBranch <- optionMaybe elseParser
  return $ If conditional trueBranch falseBranch

whileStmt wFunc = do
  reserved "while"
  conditional <- parens expression
  body <- (statementsBlock wFunc True)
  return $ While conditional body

returnStmt = reserved "return" >> (Return <$> (optionMaybe expression))
nonlocalStmt = reserved "nonlocal" >> (Nonlocal <$> identifier)
funcOnlyStmt = try returnStmt <|> try nonlocalStmt

continueStmt = reserved "continue" >> return Continue
breakStmt = reserved "break" >> return Break
whileOnlyStmt = try breakStmt <|> try continueStmt

flowStmt' wFunc wWhile =  try (ifStmt wFunc wWhile)
                      <|> try (whileStmt wFunc)
                      <|> try assignStmt

flowStmt False False = flowStmt' False False
flowStmt True False = try funcOnlyStmt <|> try (flowStmt' True False)
flowStmt False True = try whileOnlyStmt <|> try (flowStmt' False True)
flowStmt True True = try funcOnlyStmt <|> try whileOnlyStmt <|> try (flowStmt' True True)

expressionStmt :: Parser Statement
expressionStmt = Expression <$> expression

statement :: Bool -> Bool -> Parser Statement
statement wFunc wWhile =  try expressionStmt
                      <|> try (flowStmt wFunc wWhile)


-- ############################
-- ### TOPLEVEL EXPRESSIONS ###
-- ############################

endsWithBlock :: Statement -> Bool
endsWithBlock (While _ _)              = True
endsWithBlock (If _ _ _)               = True
endsWithBlock (Expression (Function _ _ _)) = True
endsWithBlock _                        = False


-- Parser modificator that ensures what expressions should end with semicolon on top-level
ensureSemi :: Parser Statement -> Parser Statement
ensureSemi stmtParser = do
  stmt <- stmtParser
  case stmt of
    (endsWithBlock -> True) -> return stmt
    _                        -> (reservedOp ";" >> return stmt)

-- Parses many expressions of given parser with correct semicolons
toplevelProducer :: Parser Statement -> Parser [Statement]
toplevelProducer = many . ensureSemi

-- Blocks ( { } ) parsers for given allowed expressions
statementsBlock wFunc wWhile =
      try (braces $ toplevelProducer (statement wFunc wWhile))
  <|> try (blockExprAsSingle $ ensureSemi (statement wFunc wWhile))

-- Helper function to allow single expression as block
blockExprAsSingle :: Parser Statement -> Parser [Statement]
blockExprAsSingle parser = return <$> parser

-- Handle of whitespaces and EOF
contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r

-- Parse given string or return an error
parseToplevel :: String -> Either ParseError [Statement]
parseToplevel s = parse (contents $ toplevelProducer (statement False False)) "<stdin>" s
