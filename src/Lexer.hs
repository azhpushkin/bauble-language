module Lexer where

import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()

lexer = Tok.makeTokenParser languageDef
  where
    ops = ["+", "*", "-", "/",
           "<", "<=", ">", ">=", "==", "!=",
           "and", "or", "not", "="]
    names = ["true", "false", "null",  -- predefined values
             "if", "else",  -- if-expression
             "import", "as",  -- imports
             "function", "return", -- function declaration
             "while", "break", "continue",  -- while-expression
             "print", "isnull", "length"  -- builtin functions
            ]
    languageDef = emptyDef {
                Tok.commentLine = "//"
              , Tok.commentStart = "/*"
              , Tok.commentEnd = "*/"
              , Tok.reservedOpNames = ops
              , Tok.reservedNames = names
              , Tok.identStart = letter
              , Tok.identLetter = alphaNum
              }

integer = Tok.integer lexer
float = Tok.float lexer
stringLiteral = Tok.stringLiteral lexer

identifier = Tok.identifier lexer

parens = Tok.parens lexer
braces = Tok.braces lexer
brackets = Tok.brackets lexer

semiSep = Tok.semiSep lexer
commaSep = Tok.commaSep lexer
semi = Tok.semi lexer
dot = Tok.dot lexer
whiteSpace = Tok.whiteSpace lexer

reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
