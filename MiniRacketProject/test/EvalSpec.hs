-- test/EvalSpec.hs
module EvalSpec where

import Test.Hspec
import Expr (Value(..))
import Eval (evalString)
import Error (ErrorType)

spec :: Spec
spec = do
    describe "eval expressions" $ do
        it "evaluates a variable via let" $
            evalString "(let (x 10) x)" `shouldBe` Right (IntValue 10)

        it "evaluates (if true 1 0)" $
            evalString "(if true 1 0)" `shouldBe` Right (IntValue 1)

        it "evaluates (if false 1 0)" $
            evalString "(if false 1 0)" `shouldBe` Right (IntValue 0)

        it "evaluates negated variable (let (x 5) -x)" $
            evalString "(let (x 5) -x)" `shouldBe` Right (IntValue (-5))

        it "evaluates number: 1235" $ 
            evalString "1235" `shouldBe` Right (IntValue 1235)

        it "evaluates negative numbers: -12235" $
            evalString "-12235" `shouldBe` Right (IntValue (-12235))

        it "evaluates true" $
            evalString "true" `shouldBe` Right (BoolValue True)

        it "evaluates false" $
            evalString "false" `shouldBe` Right (BoolValue False)

        it "evaluates (and true true)" $
            evalString "(and true true)" `shouldBe` Right (BoolValue True)

        it "evaluates (and true false)" $
            evalString "(and true false)" `shouldBe` Right (BoolValue False)

        it "evaluates (or false false)" $
            evalString "(or false false)" `shouldBe` Right (BoolValue False)

        it "evaluates (or false true)" $
            evalString "(or false true)" `shouldBe` Right (BoolValue True)

        it "evaluates (not false)" $
            evalString "(not false)" `shouldBe` Right (BoolValue True)

        it "evaluates (not true)" $
            evalString "(not true)" `shouldBe` Right (BoolValue False)

        it "evaluates (+ 2 3)" $
            evalString "(+ 2 3)" `shouldBe` Right (IntValue 5)

        it "evaluates (- 10 3)" $
            evalString "(- 10 3)" `shouldBe` Right (IntValue 7)

        it "evaluates (* 4 2)" $
            evalString "(* 4 2)" `shouldBe` Right (IntValue 8)

        it "evaluates (div 10 2)" $
            evalString "(div 10 2)" `shouldBe` Right (IntValue 5)

        it "evaluates (mod 10 3)" $
            evalString "(mod 10 3)" `shouldBe` Right (IntValue 1)

        it "evaluates (equal? 3 4)" $
            evalString "(equal? 3 4)" `shouldBe` Right (BoolValue False)

        it "evaluates (< 2 5)" $
            evalString "(< 2 5)" `shouldBe` Right (BoolValue True)

        it "evaluates (< 5 2)" $
            evalString "(< 5 2)" `shouldBe` Right (BoolValue False)

        it "evaluates (equal? 5 5)" $
            evalString "(equal? 5 5)" `shouldBe` Right (BoolValue True)
