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
    it "`print` is parses as builtin" $ do
      "print;" `shouldParseTo` [BuiltinRef Print]
      "x=print;x();" `shouldParseTo` [Assign "x" (BuiltinRef Print), Call (Variable "x") []]

    it "`self` is parses as builtin allowed in function only" $ do
      shouldFail "self;"
      "function() {self;}" `shouldParseTo` [Function [] [BuiltinRef Self]]

    it "`self` is parses as builtin allowed in function only" $ do
      shouldFail "self;"
      "function() {self;}" `shouldParseTo` [Function [] [BuiltinRef Self]]

    it "`self` and `print` not allowed for assignment" $ do
      shouldFail "function() {self = 1;}"
      shouldFail "print = 1;"

    it "Call could be operand of operator" $ do
      "1 + some()" `shouldParseTo` [BinOperator Plus
                                                (Value $ Number $ Integer 1)
                                                (Call (Variable "some") [])]
      "function() {1 + self();}" `shouldParseTo` [BinOperator Plus
                                                              (Value $ Number $ Integer 1)
                                                              (Call (BuiltinRef Self) [])]
      "not foo();" `shouldParseTo` [UnOperator Not (Call (Variable "foo") [])]


