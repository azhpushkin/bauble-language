{-# LANGUAGE ViewPatterns #-}

module ASTBuildingSpec where

import Data.Either
import Text.Parsec
import Test.Hspec

import Parser
import Syntax

successfullyParsed :: String -> [Expr]
successfullyParsed (parseToplevel -> Left err) = error $ "Error occured! " ++ show err
successfullyParsed (parseToplevel -> Right exprs) = exprs

errorOfParsing :: String -> String
errorOfParsing (parseToplevel -> Left err) = show err
errorOfParsing (parseToplevel -> Right exprs) = error $ "Error not occured, AST is " ++ show exprs


spec :: Spec
spec = do
  describe "process" $ do
    it "1. Empty `return` is allowed" $ do
      let result = Function [] [(Return Nothing)]
      successfullyParsed "function() {return;}" `shouldBe` [result]

    it "2. `return` in `if` expression, if without braces" $ do
      let programText = "function () {\
                        \  if (true) return true;\
                        \  else return false;\
                        \}"
      let result = Function [] [(If (Value (Boolean True))
                                    [(Return $ Just (Value (Boolean True)))]
                                    (Just [(Return $ Just (Value (Boolean False)))]))]
      successfullyParsed programText `shouldBe` [result]

    it "3. `return` in `while` expression, while without braces" $ do
      let programText = "function () {\
                        \  while(false)\
                        \    return 1;\
                        \}"
      let result = Function [] [(While (Value (Boolean False))
                                       [(Return $ Just (Value (Integer 1)))])]
      successfullyParsed programText `shouldBe` [result]


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
                     (Function
                        []
                        [(While
                            (Value $ Boolean False)
                            [If (Value $ Boolean True)
                                [(Function [] [(Return $ Just (Value $ Boolean True))])]
                                (Just [While (Value $ Boolean False)
                                             [(Value $ Boolean False)]])])])

      successfullyParsed programText `shouldBe` [result]

    it "5. Chained calls" $ do
      let programText = "(x()(1)) (function() {})(true);"
      let firstCall = Call (Variable "x") []
      let secondCall = Call firstCall [(Value $ Integer 1)]
      let thirdCall = Call secondCall [(Function [] [])]
      let lastCall = Call thirdCall [(Value $ Boolean True)]
      successfullyParsed programText `shouldBe` [lastCall]


    it "6. `break` and `continue` allowed in if block" $ do
      let programText = "while (true)\
                        \  if (1) break;\
                        \  else continue;"

      let result = While (Value $ Boolean True)
                         [(If (Value $ Integer 1)
                             [Break]
                             (Just [Continue]))]
      successfullyParsed programText `shouldBe` [result]

    it "7. `break` not allowed in function inside while" $ do
      let programText = "while (true) function() break;"
      errorOfParsing programText `shouldBe` "asd"

    it "8. `continue` not allowed in function inside while" $ do
      let programText = "while (true) function() break;"
      errorOfParsing programText `shouldBe` "aaaaa"

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

      let result = Function [] [(While (Value $ Boolean True)
                                       [If (Value $ Boolean True)
                                           [(Return Nothing), Break, Continue]
                                           Nothing])]
      successfullyParsed programText `shouldBe` [result]
