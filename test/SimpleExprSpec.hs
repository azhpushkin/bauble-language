{-# LANGUAGE ViewPatterns #-}

module SimpleExprSpec where

import Data.Either
import Data.List
import Text.Parsec
import Test.Hspec

import Parser
import Syntax

shouldParseTo (parseToplevel -> Left err) expected = error $ "Error occured! " ++ show err
shouldParseTo (parseToplevel -> Right stmts) expected = stmts `shouldBe` expected

shouldFail (parseToplevel -> Left err) = 1 `shouldBe` 1
shouldFail (parseToplevel -> Right stmts) = error $ "Error not occured, AST is " ++ show stmts


spec :: Spec
spec = do
  describe "process" $ do

    it "`isnull` parses as builtin function" $ do
      "isnull;" `shouldParseTo` [Expression (Value (BuiltinFunction IsNull))]

    -- it "`print` parses as builtin function" $ do
    --   "print(\"hello\", 1);" `shouldParseTo` [Expression (Call
    --                                                         (Value (BuiltinFunction Print))
    --                                                         [(Value (String "hello"))
    --                                                          (Value (Integer 1))])]

    -- it "Function name is optional" $ do
    --   "function() {}" `shouldParseTo` [Function Nothing [] []]
    --   "function foo() {}" `shouldParseTo` [Function (Just "foo") [] []]

    -- it "`print` not allowed for assignment or as simple expression" $ do
    --   shouldFail "print = 1;"
    --   shouldFail "x = print;"

    -- it "Call could be right operand of operator" $ do
    --   "1 + some();" `shouldParseTo` [BinOperator Plus
    --                                             (numExpr $ Integer 1)
    --                                             (Call (Variable "some") [])]

    -- it "Lambda-call could be right operand of operator" $ do
    --   "1 + (function () {})();" `shouldParseTo` [BinOperator Plus
    --                                             (numExpr $ Integer 1)
    --                                             (Call (Function Nothing [] []) [])]

    -- it "Call could be left operand of operator" $ do
    --   "some() + 1;" `shouldParseTo` [BinOperator Plus
    --                                             (Call (Variable "some") [])
    --                                             (numExpr $ Integer 1)]

    -- it "Lambda-call could be left operand of operator" $ do
    --   "(function () {})() / 1;" `shouldParseTo` [BinOperator Divide
    --                                             (Call (Function Nothing [] []) [])
    --                                             (numExpr $ Integer 1)]

    -- it "Call could be left operand of unary operators" $ do
    --   "not foo();" `shouldParseTo` [UnOperator Not (Call (Variable "foo") [])]
    --   "- foo();" `shouldParseTo` [UnOperator Negate (Call (Variable "foo") [])]

    -- it "Lambda-call could be left operand of unary operators" $ do
    --   "not (function () {})();" `shouldParseTo` [UnOperator Not (Call (Function Nothing [] []) [])]
    --   "- (function () {})();" `shouldParseTo` [UnOperator Negate (Call (Function Nothing [] []) [])]

    -- it "Lambda-call allowed only in parentheses" $ do
    --   "(function () {}) ()  ();" `shouldParseTo` [(Call (Call (Function Nothing [] []) []) [])]
    --   shouldFail "function () {} ();"

    -- it "Lambda-call as argument of lambda-call" $ do
    --   let res = Call (Function Nothing [] []) [(Call (Function Nothing [] []) [])]
    --   "(function () {}) ((function () {}) ());" `shouldParseTo` [res]

    -- it "`if`, `function` and `while` does not require semicolon" $ do
    --   shouldFail "function () {};"
    --   shouldFail "if (1) {};"
    --   shouldFail "if (1) {} else {};"
    --   shouldFail "while (1) {};"

    --   "function () {}" `shouldParseTo` [Function Nothing [] []]
    --   "if (1) {}" `shouldParseTo` [If (numExpr $ Integer 1) [] Nothing]
    --   "if (1) {} else {}" `shouldParseTo` [If (numExpr $ Integer 1) [] (Just [])]
    --   "while (1) {}" `shouldParseTo` [While (numExpr $ Integer 1) []]

