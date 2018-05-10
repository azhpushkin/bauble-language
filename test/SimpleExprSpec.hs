{-# LANGUAGE ViewPatterns #-}

module SimpleExprSpec where

import Data.Either
import Data.List
import Text.Parsec
import Test.Hspec

import Parser
import Syntax

shouldParseTo (parseToplevel -> Left err) expected = error $ "Error occured! " ++ show err
shouldParseTo (parseToplevel -> Right exprs) expected = exprs `shouldBe` expected

shouldFail (parseToplevel -> Left err) = 1 `shouldBe` 1
shouldFail (parseToplevel -> Right exprs) = error $ "Error not occured, AST is " ++ show exprs


spec :: Spec
spec = do
  describe "process" $ do

    it "`print` is parses as separate expression" $ do
      shouldFail "print;"
      "print(\"hello\", 1);" `shouldParseTo` [Print [Right "hello", Left (numExpr $ Integer 1)]]

    it "Function name is optional" $ do
      "function() {}" `shouldParseTo` [Function Nothing [] []]
      "function foo() {}" `shouldParseTo` [Function (Just "foo") [] []]

    it "`print` not allowed for assignment or as simple expression" $ do
      shouldFail "print = 1;"
      shouldFail "x = print;"

    it "Call could be right operand of operator" $ do
      "1 + some();" `shouldParseTo` [BinOperator Plus
                                                (numExpr $ Integer 1)
                                                (Call (Variable "some") [])]

    it "Call could be left operand of operator" $ do
      "some() + 1;" `shouldParseTo` [BinOperator Plus
                                                (Call (Variable "some") [])
                                                (numExpr $ Integer 1)]

    it "Call could be left operand of unary operators" $ do
      "not foo();" `shouldParseTo` [UnOperator Not (Call (Variable "foo") [])]
      "- foo();" `shouldParseTo` [UnOperator Negate (Call (Variable "foo") [])]

