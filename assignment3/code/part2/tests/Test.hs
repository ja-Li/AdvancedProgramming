-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000000) tests

tests = testGroup "Minimal tests" [
  testCase "simple success" $
    parseString "2 + two" @?=
      Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
  testCase "error number" $
    case parseString "-045" of
        Left e -> return ()
        Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "number" $
    parseString "-0" @?= 
      Right [SExp (Const (IntVal 0))],
  testCase "constString" $
    parseString "notssss" @?=
      Right [SExp (Var "notssss")],
  testCase "complex constString" $
    parseString "'not\\'\\n\\\\ccc'" @?=
      Right [SExp (Const (StringVal "not'\n\\ccc"))],
  testCase "error constString" $
    case parseString "'not\\n\\s'" of
        Left e -> return ()
        Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "Ident" $
    parseString "notx" @?= Right [SExp (Var "notx")],
  testCase "ErrorIdent" $
    case parseString "for" of 
        Left e -> return ()
        Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "test not in" $
    parseString "[(x)not\tin(not(y))]" @?=
      Right [SExp (List [Not (Oper In (Var "x" ) (Not (Var "y" )))])],
  testCase "miss sapce before in" $
    case parseString "xin 4" of 
        Left e -> return ()
        Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "miss sapce after in" $
    case parseString "x in4" of 
        Left e -> return ()
        Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "for and if" $
    parseString "[(x)for\ty\tin[z]if(u)]" @?=
      Right [SExp (Compr (Var "x" ) [CCFor "y" (List [Var "z" ]),CCIf (Var "u" )])],
  testCase "Comments" $
    parseString "#this is a comments\n#foo#1#ccc\n" @?=
      Right [SExp (Const (IntVal 1))],
  testCase "Comments as separator" $
    parseString "not#ccccc\ncool" @?=
      Right [SExp (Not (Var "cool" ))],
  testCase "Test fundmental Oper" $
      parseString "x * y + 5 - 3 % 1 + 1 // 7" @?= 
        Right [SExp (Oper Plus (Oper Minus (Oper Plus (Oper Times (Var "x") (Var "y")) (Const (IntVal 5))) (Oper Mod (Const (IntVal 3)) (Const (IntVal 1)))) (Oper Div (Const (IntVal 1)) (Const (IntVal 7))))],
  testCase "Disambiguation not" $
    parseString "not x< y+z*4" @?=
      Right [SExp (Not (Oper Less (Var "x" ) (Oper Plus (Var "y" ) (Oper Times (Var "z" ) (Const (IntVal 4) )))))],
  testCase "Disambiguation multoper" $
    parseString "x%y-z>(not u)" @?=
      Right [SExp (Oper Greater (Oper Minus (Oper Mod (Var "x" ) (Var "y" )) (Var "z" )) (Not (Var "u" )))],
  testCase "associativity" $
    case parseString "x=x=x)" of
        Left e -> return ()
        Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "Opers which can not be combined >= <=" $
    case parseString "x >= y <= z" of 
        Left e -> return ()
        Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "Opers which can not be combined !=" $
    case parseString "x != y != z" of 
        Left e -> return ()
        Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  
  testCase "simple failure" $
    -- avoid "expecting" very specific parse-error messages
    case parseString "wow!" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p]
  
  
