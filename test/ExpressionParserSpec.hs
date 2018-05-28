{-# LANGUAGE ViewPatterns #-}

module ExpressionParserSpec where

import Data.Either
import Data.List
import Text.Parsec
import Test.Hspec

import Parser
import Syntax

expressionParser :: String -> Either ParseError Expression
expressionParser input = parse (contents expression) "stdin" input

shouldParseTo (expressionParser -> Left err) expected = error $ "Error occured! " ++ show err
shouldParseTo (expressionParser -> Right expr) expected = expr `shouldBe` expected

shouldFail (parseToplevel -> Left err) = 1 `shouldBe` 1
shouldFail (parseToplevel -> Right stmts) = error $ "Error not occured, AST is " ++ show stmts

-- !!! WARNING !!!
-- Function body should always be empty to avoid statements check


spec :: Spec
spec = do
  describe "process" $ do

    it "`isnull` parses as builtin function" $ do
      "isnull" `shouldParseTo` (Value (BuiltinFunction IsNull))
      "isnull(x)" `shouldParseTo` (Call (Value (BuiltinFunction IsNull)) [Variable "x"])

    it "`print` parses as builtin function" $ do
      "print(\"hello\", 1)" `shouldParseTo` (Call (Value (BuiltinFunction Print))
                                                  [Value (String "hello"), Value (Integer 1)])

    it "Function name is optional" $ do
      "function() {}" `shouldParseTo` (Function Nothing [] [])
      "function foo() {}" `shouldParseTo` (Function (Just "foo") [] [])

    it "Call could be right operand of operator" $ do
      "1 + some()" `shouldParseTo` (BinaryOp Plus
                                             (Value $ Integer 1)
                                             (Call (Variable "some") []))

    it "Lambda-call could be right operand of operator" $ do
      "1 + (function () {})()" `shouldParseTo` (BinaryOp Plus
                                                         (Value $ Integer 1)
                                                         (Call (Function Nothing [] []) []))

    it "Call could be left operand of operator" $ do
      "some() + 1" `shouldParseTo` (BinaryOp Plus
                                             (Call (Variable "some") [])
                                             (Value $ Integer 1))

    it "Lambda-call could be left operand of operator" $ do
      "(function () {})() / 1" `shouldParseTo` (BinaryOp Divide
                                                         (Call (Function Nothing [] []) [])
                                                         (Value $ Integer 1))

    it "Call could be left operand of unary operators" $ do
      "not foo()" `shouldParseTo` (UnaryOp Not (Call (Variable "foo") []))
      "- foo()" `shouldParseTo` (UnaryOp Negate (Call (Variable "foo") []))

    it "Lambda-call could be left operand of unary operators" $ do
      "not (function () {})()" `shouldParseTo` (UnaryOp Not (Call (Function Nothing [] []) []))
      "- (function () {})()" `shouldParseTo` (UnaryOp Negate (Call (Function Nothing [] []) []))

    it "Lambda-call allowed only in parentheses" $ do
      "(function () {}) ()  ()" `shouldParseTo` (Call (Call (Function Nothing [] []) []) [])
      shouldFail "function () {} ();"

    let array = (Value . Array) :: [Expression] -> Expression
    it "Simple array expressions" $ do
      "[]" `shouldParseTo` (array [])
      "[1]" `shouldParseTo` (array [(Value $ Integer 1)])
      "[x, print]" `shouldParseTo` (array [ (Variable "x")
                                          , (Value $ BuiltinFunction Print)])

      shouldFail "[x, print, ]"  -- TRAILING COMMAS GOODBYE

    it "Array with call and as call argument" $ do
      "[x(), y([4.2, null])]" `shouldParseTo` array [ (Call (Variable "x") [])
                                                    , (Call (Variable "y")
                                                            [(array [ (Value $ Double 4.2)
                                                                    , (Value Null)])])]

    it "Array as operand" $ do
      "[1 + 2]" `shouldParseTo` array [BinaryOp Plus (Value $ Integer 1)
                                                     (Value $ Integer 2)]

      "[] + [2]" `shouldParseTo` (BinaryOp Plus (array []) (array [(Value $ Integer 2)]))
      "x() + []" `shouldParseTo` (BinaryOp Plus (Call (Variable "x") [])
                                                (array []))
      "[print] + x()" `shouldParseTo` (BinaryOp Plus (array [Value $ BuiltinFunction Print])
                                                     (Call (Variable "x") []))


