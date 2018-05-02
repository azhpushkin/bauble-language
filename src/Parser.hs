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

-- Values and identifiers

integerValue = do
  n <- integer
  return $ Value $ Integer n

doubleValue = do -- For parsec float is just alias of 'non-integer'
  n <- float
  return $ Value $ Double n

trueValue = do
  reserved "true"
  return $ Value $ Boolean True

falseValue = do
  reserved "false"
  return $ Value $ Boolean False

nullValue = do
  reserved "null"
  return $ Value Null

variable = Variable <$> identifier

atomicExpr =  try doubleValue
          <|> try integerValue
          <|> try trueValue
          <|> try falseValue
          <|> try nullValue
          <|> try variable


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

operatorExpr = Ex.buildExpressionParser operatorsTable operandExpr'

-- Everything that possible could be used in operators: everything but not function or assign
-- Helper for `operatorExpr` parser
operandExpr' =  try atomicExpr
             <|> try callExpr
             <|> try (parens operatorExpr)
             <|> try (parens operandExpr')


-- Everything that possible could be called: everything but not assign or operatorExpr
-- Helper for `callExpr` parser
callableExpr' =  try variable
             <|> try functionExpr
             <|> try (parens callableExpr')

-- Helper function, that detectes call (parens and comma-separated values)
checkNextCalls :: Expr -> Parser Expr
checkNextCalls call = do
  nextArgs <- optionMaybe (parens $ commaSep simpleExpr)
  case nextArgs of
    Nothing   -> return $ call
    Just args -> do nextCall <- checkNextCalls (Call call args)
                    return nextCall


callExpr = do
  callable <- callableExpr'
  args <- parens $ commaSep simpleExpr
  let initialCall = (Call callable args)
  nextCalls <- checkNextCalls initialCall
  return nextCalls

assignExpr = do
  var <- identifier
  reservedOp "="
  someExpr <- simpleExpr
  return $ Assign var someExpr

functionExpr = do
  let
    returnExpr = do
      reserved "return"
      stmt <- simpleExpr
      return $ Return stmt

  reserved "function"
  args <- parens $ commaSep identifier
  body <- braces $ toplevelProducer (try expr <|> try returnExpr)
  return $ Function args body

simpleExpr =  try assignExpr
          <|> try functionExpr
          <|> try callExpr
          <|> try operatorExpr
          <|> parens simpleExpr


-- ########################
-- ### FLOW EXPRESSIONS ###
-- ########################

ifExpr = do
  reserved "if"
  conditional <- parens simpleExpr
  trueBranch <- blockExpr
  reserved "else"
  falseBranch <- blockExpr
  return $ If conditional trueBranch falseBranch

whileExpr = do
  reserved "while"
  conditional <- parens simpleExpr
  body <- blockExpr
  return $ While conditional body

flowExpr =  try ifExpr
        <|> try whileExpr

-- ############################
-- ### TOPLEVEL EXPRESSIONS ###
-- ############################

blockExpr :: Parser [Expr]
blockExpr = braces toplevel

toplevel = toplevelProducer expr

toplevelProducer exprParser = many $ do
  newExpr <- exprParser
  case newExpr of
    While _ _ -> return newExpr
    If _ _ _ ->return newExpr
    Function _ _ ->return newExpr
    _ -> do reservedOp ";"
            return newExpr


expr :: Parser Expr
expr = try simpleExpr <|> try flowExpr


contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r

parseToplevel :: String -> Either ParseError [Expr]-- Toplevel applies
parseToplevel s = parse (contents toplevel) "<stdin>" s





















--
--
--
--  operatorExpr = Ex.buildExpressionParser operatorsTable simpleExpr'
--
--
--
--  -- Function call
--
--  call :: Parser Expr
--  call = do
--    callable <- try variable <|> try (parens function)
--    args <- parens $ commaSep simpleExpr
--    return $ Call callable args
--
--
--  -- Helper parser for operators table
--  simpleExpr' =  try function
--             <|> try call
--             <|> try variable
--             <|> try value
--             <|> try (parens operatorExpr)
--
--  -- Expressions that can be returned
--  simpleExpr =  try operatorExpr
--            <|> try simpleExpr'
--            <|> try (parens simpleExpr')
--
--  -- Flow expressions
--  flowExpr = try assign
--         <|> try whileExpr
--         <|> try ifExpr
--
--  anyExpr :: Parser Expr
--  anyExpr = do
--    newExpr <- try flowExpr <|>
--               try operatorExpr <|>
--               try simpleExpr
--    case newExpr of
--         Function _ _ -> return newExpr
--         While _ _    -> return newExpr
--         If _ _ _     -> return newExpr
--         _            -> do reservedOp ";"
--                            return newExpr
--
--  toplevel :: Parser [Expr]
--  toplevel = many anyExpr
--
