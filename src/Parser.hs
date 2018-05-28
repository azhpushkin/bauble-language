{-# LANGUAGE ViewPatterns #-}

module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tk

import Lexer
import Syntax

-- ##########################
-- ### ATOMIC EXPRESSIONS ###
-- ##########################

-- Atomic expression is one that cannot be split up to more ones
-- Example: 1, 1.1, true, false, null, foo (values and identifiers)

integerExpr = (Value . Integer) <$> integer
doubleExpr = (Value . Double) <$> float

trueExpr = reserved "true" >> (return $ Value $ Boolean True)
falseExpr = reserved "false" >> (return $ Value $ Boolean False)

nullExpr = reserved "null" >> (return $ Value Null)

stringExpr = (Value . String) <$> stringLiteral

printExpr = reserved "print" >> (return $ Value $ BuiltinFunction Print)
isNullExpr = reserved "isnull" >> (return $ Value $ BuiltinFunction IsNull)
builinFunctionExpr = try printExpr <|> try isNullExpr

variableExpr = identifier >>= (\var -> checkNextCalls (Variable var))

atomicExpr =  try doubleExpr
          <|> try integerExpr
          <|> try trueExpr
          <|> try falseExpr
          <|> try nullExpr
          <|> try stringExpr
          <|> try builinFunctionExpr
          <|> try variableExpr

-- ##########################
-- ### SIMPLE EXPRESSIONS ###
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

operatorExpr = Ex.buildExpressionParser operatorsTable operandExpr'

-- Everything that possible could be used in operators: everything but not function or assign
-- Helper for `operatorExpr` parser
operandExpr' =  try lambdaExprCall
            <|> try atomicExpr
            <|> try (parens operatorExpr)

-- Helper function, that detectes call (parens and comma-separated values)
checkNextCalls :: Expression -> Parser Expression
checkNextCalls call = do
  nextArgs <- optionMaybe (parens $ commaSep simpleExpr)
  case nextArgs of
    Nothing   -> return $ call
    Just args -> checkNextCalls (Call call args)

lambdaExprCall = (parens functionExpr) >>= checkNextCalls

assignExpr = do
  var <- identifier
  reservedOp "="
  someExpr <- simpleExpr
  return $ Assign var someExpr

-- Always starts new scope, so, not bool params required
functionExpr = do
  reserved "function"
  selfRef <- optionMaybe identifier
  args <- parens $ commaSep identifier
  body <- (blockExpr True False)
  notFollowedBy $ (parens $ commaSep identifier)
  return $ Function selfRef args body

callableExpr' =  try lambdaExprCall
             <|> try functionExpr
             <|> try variableExpr
             <|> try builinFunctionExpr
             <|> try (parens callableExpr')

nonCallableExpr' =  try operatorExpr
                <|> try atomicExpr
                <|> try (parens nonCallableExpr')

simpleExpr :: Parser Expression
simpleExpr = (try nonCallableExpr' <|> try callableExpr') >>= checkNextCalls


-- ########################
-- ### FLOW EXPRESSIONS ###
-- ########################

importStmt = reserved "import" >> (Import <$> identifier)

ifExpr withinFunc withinWhile = do
  reserved "if"
  conditional <- parens simpleExpr
  trueBranch <- (blockExpr withinFunc withinWhile)
  falseBranch <- optionMaybe (reserved "else" >> (blockExpr withinFunc withinWhile))
  return $ If conditional trueBranch falseBranch

whileExpr withinFunc = do
  reserved "while"
  conditional <- parens simpleExpr
  body <- (blockExpr withinFunc True)
  return $ While conditional body


returnExpr = reserved "return" >> (Return <$> (optionMaybe simpleExpr))
nonlocalExpr = reserved "nonlocal" >> (Nonlocal <$> identifier)
funcOnlyExpr = try returnExpr <|> try nonlocalExpr

continueExpr = reserved "continue" >> return Continue
breakExpr = reserved "break" >> return Break
whileOnlyExpr = try breakExpr <|> try continueExpr

flowExpr' wFunc wWhile =  try (ifExpr wFunc wWhile) 
                      <|> try (whileExpr wFunc)
                      <|> try importStmt
                      <|> try assignExpr

flowExpr :: Bool -> Bool -> Parser Statement
flowExpr False False = flowExpr' False False
flowExpr True False = try funcOnlyExpr <|> try (flowExpr' True False)
flowExpr False True = try whileOnlyExpr <|> try (flowExpr' False True)
flowExpr True True = try funcOnlyExpr <|> try whileOnlyExpr <|> try (flowExpr' True True)


-- ############################
-- ### TOPLEVEL EXPRESSIONS ###
-- ############################

endsWithBlock :: Statement -> Bool
endsWithBlock (While _ _)      = True
endsWithBlock (If _ _ _)       = True
endsWithBlock (Expression (Function _ _ _)) = True
endsWithBlock _                = False

-- General parser to check all expressions available on top-level
simpleStmt = Expression <$> simpleExpr
stmt wFunc wWhile = try (flowExpr wFunc wWhile) <|> try simpleStmt

-- Parser modificator that ensures what expressions should end with semicolon on top-level
ensureSemi :: Parser Statement -> Parser Statement
ensureSemi exprParser = do
  newExpr <- exprParser
  case newExpr of
    (endsWithBlock -> True)                 -> return newExpr
    _                                       -> (reservedOp ";" >> return newExpr)

-- Parses many expressions of given parser with correct semicolons
toplevelProducer :: Parser Statement -> Parser [Statement]
toplevelProducer = many . ensureSemi

-- Blocks ( { } ) parsers for given allowed expressions
blockExpr wFunc wWhile =  try (braces $ toplevelProducer (stmt wFunc wWhile))
                      <|> try (blockExprAsSingle $ ensureSemi (stmt wFunc wWhile))

-- Helper function to allow single expression as block
blockExprAsSingle :: Parser Statement -> Parser [Statement]
blockExprAsSingle exprParser = return <$> exprParser

-- Handle of whitespaces and EOF
contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r

-- Parse given string or return an error
parseToplevel :: String -> Either ParseError [Statement]
parseToplevel s = parse (contents $ toplevelProducer (stmt False False)) "<stdin>" s