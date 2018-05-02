module ASTBuildingSpec where

import Data.Either
import Text.Parsec
import Test.Hspec

import Parser
import Syntax

parseLine :: String -> Expr
parseLine line = case parseToplevel line of
  Left _ -> Return $ Value Null
  Right (expr:_) -> expr

spec :: Spec
spec = do
  describe "process" $ do
    it "Chain of calls" $ do
      let result = Call (Call (Variable "x") [Value (Integer 1)]) []
      parseLine "x(1)();" `shouldBe` result

    it "Function assignment" $ do
      let result = Assign "foo" (Function [] [(Return (Variable "bar"))])
      parseLine "foo = function() {return bar;}" `shouldBe` result
