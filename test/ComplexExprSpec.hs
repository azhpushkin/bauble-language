{-# LANGUAGE ViewPatterns #-}

module ComplexExprSpec where

import Data.Either
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
    it "1. Empty `return` is allowed" $ do
      let result = Function Nothing [] [(Return Nothing)]
      "function() {return;}" `shouldParseTo` [result]

    it "2. `return` in `if` expression, if without braces" $ do
      let programText = "function () {\
                        \  if (true) return true;\
                        \  else return false;\
                        \}"
      let result = Function Nothing [] [(If (Value (Boolean True))
                                        [(Return $ Just (Value (Boolean True)))]
                                        (Just [(Return $ Just (Value (Boolean False)))]))]
      programText `shouldParseTo` [result]

    it "3. `return` in `while` expression, while without braces" $ do
      let programText = "function () {\
                        \  while(false)\
                        \    return 1;\
                        \}"
      let result = Function Nothing [] [(While (Value (Boolean False))
                                         [(Return $ Just (numExpr (Integer 1)))])]
      programText `shouldParseTo` [result]


    it "4. `while`, `if` and `function` expressions with blocks without braces" $ do
      let programText = "foo = function () {\
                        \  while(false)\
                        \    if(true)\
                        \      function() return true;\
                        \    else\
                        \      while (false)\
                        \        false;\
                        \}"
      let result = Assign
                     "foo"
                     (Function Nothing
                        []
                        [(While
                            (Value $ Boolean False)
                            [If (Value $ Boolean True)
                                [(Function Nothing [] [(Return $ Just (Value $ Boolean True))])]
                                (Just [While (Value $ Boolean False)
                                             [(Value $ Boolean False)]])])])

      programText `shouldParseTo` [result]

    it "5. Chained calls" $ do
      let programText = "(x()(1)) (function() {})(true);"
      let firstCall = Call (Variable "x") []
      let secondCall = Call firstCall [(numExpr $ Integer 1)]
      let thirdCall = Call secondCall [(Function Nothing [] [])]
      let lastCall = Call thirdCall [(Value $ Boolean True)]
      programText `shouldParseTo` [lastCall]


    it "6. `break` and `continue` allowed in if block" $ do
      let programText = "while (true)\
                        \  if (1) break;\
                        \  else continue;"

      let result = While (Value $ Boolean True)
                         [(If (numExpr $ Integer 1)
                             [Break]
                             (Just [Continue]))]
      programText `shouldParseTo` [result]

    it "7. `break` not allowed in function inside while" $ do
      let programText = "while (true) function() break;"
      shouldFail programText

    it "8. `continue` not allowed in function inside while" $ do
      let programText = "while (true) function() break;"
      shouldFail programText

    it "9. Nested control-flow expressions, `return`, `break` and `continue` allowed" $ do
      let programText = "function() {\
                        \  while (true) {\
                        \    if (true) {\
                        \      return;\
                        \      break;\
                        \      continue;\
                        \    }\
                        \  }\
                        \}"

      let result = Function Nothing [] [(While (Value $ Boolean True)
                                           [If (Value $ Boolean True)
                                               [(Return Nothing), Break, Continue]
                                               Nothing])]
      programText `shouldParseTo` [result]
