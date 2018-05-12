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

integerValue = (Value . Number . Integer) <$> integer
doubleValue = (Value . Number . Double) <$> float
falseValue = reserved "false" >> (return $ Value $ Boolean False)
trueValue = reserved "true" >> (return $ Value $ Boolean True)
nullValue = reserved "null" >> (return $ Value Null)

variableExpr = identifier >>= (\var -> checkNextCalls (Variable var))

atomicExpr =  try doubleValue
          <|> try integerValue
          <|> try trueValue
          <|> try falseValue
          <|> try nullValue
          <|> try variableExpr

-- ##########################
-- ### SIMPLE EXPRESSIONS ###
-- ##########################

-- OPERATORS TABLE

binary s f assoc = Ex.Infix (reservedOp s >> return (BinOperator f)) assoc
unary s f = Ex.Prefix (reservedOp s >> return (UnOperator f))

numberOperatorsTable = [ [ unary  "-"  Negate]
                       , [ binary "*"  Multiply       Ex.AssocLeft
                         , binary "/"  Divide         Ex.AssocLeft]
                       , [ binary "+"  Plus           Ex.AssocRight
                         , binary "-"  Minus          Ex.AssocLeft]
                       , [ binary "<"  Less           Ex.AssocNone
                         , binary "<=" LessOrEqual    Ex.AssocNone
                         , binary "<"  Greater        Ex.AssocNone
                         , binary "<"  GreaterOrEqual Ex.AssocNone
                         , binary "==" Equal          Ex.AssocNone
                         , binary "!=" NotEqual       Ex.AssocNone] ]

boolOperatorsTable = [ [ unary "not"  Not]
                     , [ binary "and" And Ex.AssocLeft
                       , binary "or"  Or  Ex.AssocLeft] ]

operatorsTable = numberOperatorsTable ++ boolOperatorsTable

operatorExpr = Ex.buildExpressionParser operatorsTable operandExpr'

-- Everything that possible could be used in operators: everything but not function or assign
-- Helper for `operatorExpr` parser
operandExpr' =  try lambdaExprCall
            <|> try atomicExpr
            <|> try (parens operatorExpr)

-- Helper function, that detectes call (parens and comma-separated values)
checkNextCalls :: Expr -> Parser Expr
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
             <|> try (parens callableExpr')

nonCallableExpr' =  try assignExpr
                <|> try operatorExpr
                <|> try atomicExpr
                <|> try (parens nonCallableExpr')

simpleExpr = (try nonCallableExpr' <|> try callableExpr') >>= checkNextCalls


-- ########################
-- ### FLOW EXPRESSIONS ###
-- ########################

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

printableExpr :: Parser (Either Expr String)
printableExpr = do
  maybeMsg <- optionMaybe stringLiteral
  case maybeMsg of
    Just msg -> return $ Right msg
    Nothing -> Left <$> simpleExpr

printExpr = do
  reserved "print"
  args <- parens $ commaSep printableExpr
  return $ Print args

returnExpr = reserved "return" >> (Return <$> (optionMaybe simpleExpr))

continueExpr = reserved "continue" >> return Continue
breakExpr = reserved "break" >> return Break
whileOnlyExpr = try breakExpr <|> try continueExpr

flowExpr' wFunc wWhile = try (ifExpr wFunc wWhile) <|> try (whileExpr wFunc) <|> try printExpr

flowExpr False False = flowExpr' False False
flowExpr True False = try returnExpr <|> try (flowExpr' True False)
flowExpr False True = try whileOnlyExpr <|> try (flowExpr' False True)
flowExpr True True = try returnExpr <|> try whileOnlyExpr <|> try (flowExpr' True True)


-- ############################
-- ### TOPLEVEL EXPRESSIONS ###
-- ############################

endsWithBlock :: Expr -> Bool
endsWithBlock (While _ _)    = True
endsWithBlock (If _ _ _)     = True
endsWithBlock (Function _ _ _) = True
endsWithBlock _              = False

-- General parser to check all expressions available on top-level
expr wFunc wWhile = try simpleExpr <|> try (flowExpr wFunc wWhile)

-- Parser modificator that ensures what expressions should end with semicolon on top-level
ensureSemi :: Parser Expr -> Parser Expr
ensureSemi exprParser = do
  newExpr <- exprParser
  case newExpr of
    (endsWithBlock -> True)          -> return newExpr
    Assign _ (endsWithBlock -> True) -> return newExpr
    _                                -> (reservedOp ";" >> return newExpr)

-- Parses many expressions of given parser with correct semicolons
toplevelProducer :: Parser Expr -> Parser [Expr]
toplevelProducer = many . ensureSemi

-- Blocks ( { } ) parsers for given allowed expressions
blockExpr wFunc wWhile =  try (braces $ toplevelProducer (expr wFunc wWhile))
                      <|> try (blockExprAsSingle $ ensureSemi (expr wFunc wWhile))

-- Helper function to allow single expression as block
blockExprAsSingle :: Parser Expr -> Parser [Expr]
blockExprAsSingle exprParser = return <$> exprParser

-- Handle of whitespaces and EOF
contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r

-- Parse given string or return an error
parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents $ toplevelProducer (expr False False)) "<stdin>" s
