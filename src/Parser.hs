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

variableExpr = Variable <$> identifier
printExpr = reserved "print" >> (return $ BuiltinRef Print)

atomicExpr =  try doubleValue
          <|> try integerValue
          <|> try trueValue
          <|> try falseValue
          <|> try nullValue
          <|> try variableExpr
          <|> try printExpr

-- Special case: not general-allowed but still atomic expressions

selfExpr = reserved "self" >> (return $ BuiltinRef Self)

-- ##########################
-- ### SIMPLE EXPRESSIONS ###
-- ##########################

-- OPERATORS TABLE

binary s f assoc = Ex.Infix (reservedOp s >> return (BinOperator f)) assoc
unary s f = Ex.Prefix (reservedOp s >> return (UnOperator f))

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
                     , [ binary "and" And Ex.AssocLeft
                       , binary "or"  Or  Ex.AssocLeft] ]

operatorsTable = numberOperatorsTable ++ boolOperatorsTable

operatorExpr wFunc = Ex.buildExpressionParser operatorsTable (operandExpr' wFunc)

-- Everything that possible could be used in operators: everything but not function or assign
-- Helper for `operatorExpr` parser
operandExpr' withinFunc =  try (callExpr withinFunc)
                       <|> try atomicExpr
                       <|> try (parens $ operatorExpr withinFunc)
                       <|> try (parens $ operandExpr' withinFunc)


-- Everything that possible could be called: everything but not assign or operatorExpr
-- Helper for `callExpr` parser
callableExpr' =  try variableExpr
             <|> try functionExpr
             <|> try (parens callableExpr')

callableWithinFuncExpr' =  try variableExpr
                       <|> try selfExpr
                       <|> try functionExpr
                       <|> try (parens callableWithinFuncExpr')

-- Helper function, that detectes call (parens and comma-separated values)
checkNextCalls :: Expr -> Bool -> Parser Expr
checkNextCalls call withinFunc = do
  nextArgs <- optionMaybe (parens $ commaSep (simpleExpr withinFunc))
  case nextArgs of
    Nothing   -> return $ call
    Just args -> checkNextCalls (Call call args) withinFunc

callExpr withinFunc = do
  callable <- callableWithinFuncExpr'
  args <- parens $ commaSep (simpleExpr withinFunc)
  return (Call callable args)
  -- checkNextCalls (Call callable args) withinFunc

assignExpr withinFunc = do
  var <- identifier
  reservedOp "="
  someExpr <- (simpleExpr withinFunc)
  return $ Assign var someExpr

-- Always starts new scope, so, not bool params required
functionExpr = do
  reserved "function"
  args <- parens $ commaSep identifier
  body <- (blockExpr True False)
  return $ Function args body

simpleExpr withinFunc@False =  try (assignExpr withinFunc)
                           <|> try functionExpr
                           <|> try (callExpr withinFunc)
                           <|> try (operatorExpr withinFunc)
                           <|> parens (simpleExpr withinFunc)

simpleExpr True = try selfExpr <|> try (simpleExpr False)

-- ########################
-- ### FLOW EXPRESSIONS ###
-- ########################

ifExpr withinFunc withinWhile = do
  reserved "if"
  conditional <- parens (simpleExpr withinFunc)
  trueBranch <- (blockExpr withinFunc withinWhile)
  falseBranch <- optionMaybe (reserved "else" >> (blockExpr withinFunc withinWhile))
  return $ If conditional trueBranch falseBranch

whileExpr withinFunc = do
  reserved "while"
  conditional <- parens (simpleExpr withinFunc)
  body <- (blockExpr withinFunc True)
  return $ While conditional body

returnExpr = reserved "return" >> (Return <$> (optionMaybe (simpleExpr True)))

continueExpr = reserved "continue" >> return Continue
breakExpr = reserved "break" >> return Break
whileOnlyExpr = try breakExpr <|> try continueExpr


flowExpr' wFunc wWhile = try (ifExpr wFunc wWhile) <|> try (whileExpr wFunc)

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
endsWithBlock (Function _ _) = True
endsWithBlock _              = False

-- General parser to check all expressions available on top-level
expr wFunc wWhile = try (simpleExpr wFunc) <|> try (flowExpr wFunc wWhile)

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
