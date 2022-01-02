-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Stubby tests"
  [testCase "crash test" $
    execute [SExp (Call "print" [Oper Plus (Const (IntVal 2))
                                           (Const (IntVal 2))]),
             SExp (Var "hello")]
      @?= (["4"], Just (EBadVar "hello")),
   testCase "oper div test" $
    execute [SExp (Call "print" [Oper Div (Const (IntVal 1)) (Const (IntVal 1))]),SExp (Var "anything")]
      @?= (["1"], Just (EBadVar "anything")),
   testCase "oper div test" $
    execute [SExp (Call "print" [Oper Div (Const (IntVal 1)) (Const (IntVal 0))])]
      @?= ([],Just (EBadArg "this operate is illegal")),
   testCase "oper mod test" $
    execute [SExp (Call "print" [Oper Mod (Const (IntVal 1)) (Const (IntVal 0))])]
      @?= ([],Just (EBadArg "this operate is illegal")),
   testCase "range and for test" $
    execute [SDef "squares" (Compr (Oper Times (Var "x") (Var "x")) [CCFor "x" (Call "range" [Const (IntVal 9)])]),
             SExp (Call "print" [Var "squares"])]
      @?= (["[0, 1, 4, 9, 16, 25, 36, 49, 64]"],Nothing),
   testCase "range test 1" $
    execute [SDef "rangeTest" (Compr (Oper Plus (Var "x") (Var "y")) [CCFor "x" (Call "range" [Const (IntVal 0), Const (IntVal 11), Const (IntVal 5)]),CCFor "y" (Call "range" [Const (IntVal 5)])]),
            SExp (Call "print" [Var "rangeTest"])]
      @?= (["[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]"],Nothing),
   testCase "range test 2" $
    execute [SDef "rangeTest2" (Call "range" [Const (IntVal 2), Const (IntVal 1)]),SExp (Call "print" [Var "rangeTest2"])]
      @?= (["[]"],Nothing),
   testCase "execute misc.ast from handout" $
     do pgm <- read <$> readFile "examples/misc.ast"
        out <- readFile "examples/misc.out"
        execute pgm @?= (lines out, Nothing),
   testCase "execute someTests.ast from handout" $
     do pgm1 <- read <$> readFile "examples/someTests.ast"
        out1 <- readFile "examples/someTests.out"
        execute pgm1 @?= (lines out1, Nothing)     
        ]
  