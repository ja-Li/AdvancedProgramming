module ExprProperties where

import Test.QuickCheck

import ExprAst
import qualified ExprEval as E
import Data.List

expr :: Gen Expr
expr = sized exprN

str :: Gen String
str = elements ["a","b","c"]

-- exprN :: (Integer a) => a -> Gen Expr
exprN 0 = fmap Const arbitrary
exprN n = oneof [
   fmap Const arbitrary,
   fmap Var str,
   Oper Plus <$> subexpr <*> subexpr,
   Oper Minus <$> subexpr <*> subexpr,
   -- 0 * error, we need to get 0
   do 
      e1 <- subexpr `suchThat` (not . isInfixOf "Var" . show) 
      e2 <- subexpr `suchThat` (not . isInfixOf "Var" . show)
      return $ Oper Times e1 e2,
   do
      id <- str
      e1 <- subexpr `suchThat` (not . isInfixOf "Var" . show)
      body <- subexpr
      return $ Let id e1 body] 
      where subexpr = exprN $ n `div` 2

instance Arbitrary Expr where
   arbitrary = expr

   shrink(Const n) = map Const $ shrink n
   shrink(Oper op x y) = 
      [x, y] ++
      [Oper op x' y' | (x',y') <- shrink(x,y)]
   shrink(Let v e body) = 
      [e,body] ++
      [Let v e' body' | (e',body') <- shrink(e,body)]

prop_eval_simplify :: Expr -> Property
prop_eval_simplify x = E.eval(E.simplify x) mempty === E.eval x mempty

prop_eval_simplify_distribution :: Expr -> Property
prop_eval_simplify_distribution e = collect e prop_eval_simplify
