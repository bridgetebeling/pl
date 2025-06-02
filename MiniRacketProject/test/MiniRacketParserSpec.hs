module MiniRacketParserSpec where 

import Test.Hspec
import Expr 
import MiniRacketParser
import Error

type ParseResult = Either ErrorType (Expr, String)

expr :: Either ErrorType (Expr, String) -> Expr
expr (Right (e, _)) = e
expr (Left (ParseError msg)) = error msg
expr (Left err) = error ("Unexpected error: " ++ show err)

spec :: Spec 
spec = do 
    describe "parse literals" $ do
        it "parses number: 1235" $ 
            parseString "1235" `shouldBe` Right (LiteralExpr (IntValue 1235), "")

        it "parses negative numbers: -12235" $
            parseString "-12235" `shouldBe` Right (LiteralExpr (IntValue (-12235)), "")

        it "parses true" $
            parseString "true" `shouldBe` Right (LiteralExpr (BoolValue True), "")

        it "parses false" $
            parseString "false" `shouldBe` Right (LiteralExpr (BoolValue False), "")

    describe "parse boolean expressions" $ do
        it "parses (and true false)" $
            parseString "(and true false)" `shouldBe`
                Right (BoolExpr And [LiteralExpr (BoolValue True), LiteralExpr (BoolValue False)], "")

        it "parses (or false true)" $
            parseString "(or false true)" `shouldBe`
                Right (BoolExpr Or [LiteralExpr (BoolValue False), LiteralExpr (BoolValue True)], "")

    describe "parse not expression" $ do
        it "parses (not true)" $
            parseString "(not true)" `shouldBe`
                Right (NotExpr (LiteralExpr (BoolValue True)), "")

    describe "parse math expressions" $ do
        it "parses (+ 1 2 3)" $
            parseString "(+ 1 2 3)" `shouldBe`
                Right (MathExpr Add [LiteralExpr (IntValue 1), LiteralExpr (IntValue 2), LiteralExpr (IntValue 3)], "")

        it "parses (* 2 3)" $
            parseString "(* 2 3)" `shouldBe`
                Right (MathExpr Mul [LiteralExpr (IntValue 2), LiteralExpr (IntValue 3)], "")

    describe "parse comparison expressions" $ do
        it "parses (equal? 4 4)" $
            parseString "(equal? 4 4)" `shouldBe`
                Right (CompExpr Eq (LiteralExpr (IntValue 4)) (LiteralExpr (IntValue 4)), "")

        it "parses (< 2 3)" $
            parseString "(< 2 3)" `shouldBe`
                Right (CompExpr Lt (LiteralExpr (IntValue 2)) (LiteralExpr (IntValue 3)), "")
    describe "parse variables and negated variables" $ do
        it "parses var x" $
            parseString "x" `shouldBe` Right (VarExpr "x", "")

        it "parses negated var -x" $
            parseString "-x" `shouldBe`
                Right (MathExpr Sub [LiteralExpr (IntValue 0), VarExpr "x"], "")

    describe "parse if expressions" $ do
        it "parses (if true 1 2)" $
            parseString "(if true 1 2)" `shouldBe`
                Right (IfExpr (LiteralExpr (BoolValue True)) (LiteralExpr (IntValue 1)) (LiteralExpr (IntValue 2)), "")

    describe "parse let expressions" $ do
        it "parses (let (x 3) x)" $
            parseString "(let (x 3) x)" `shouldBe`
                Right (LetExpr "x" (LiteralExpr (IntValue 3)) (VarExpr "x"), "")
