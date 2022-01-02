-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return a = Comp $ \_ -> (Right a, mempty)
  m >>= f = Comp $ \e -> case runComp m e of
      (Left err, s) -> (Left err, s)
      (Right a, s) -> case runComp (f a) e of
          (Left err, s1) -> (Left err, s `mappend` s1)
          (Right a1, s1) -> (Right a1, s `mappend` s1)

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort re =  Comp $ \_ -> (Left re, mempty)

look :: VName -> Comp Value
look vn = Comp $ \xs -> case lookup vn xs of
    Nothing -> (Left (EBadVar vn), mempty)
    Just a -> (Right a, mempty)

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding x v m = Comp $ \e -> (runComp m ([(x,v)]++e)) 

output :: String -> Comp ()
output s = Comp $ \_ -> (Right (), [s])

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy v = case v of
    NoneVal -> False
    FalseVal -> False
    IntVal 0 -> False
    StringVal "" -> False
    ListVal [] -> False
    _ -> True

operate :: Op -> Value -> Value -> Either String Value
operate Plus (IntVal a) (IntVal b) = Right (IntVal (a+b))
operate Minus (IntVal a) (IntVal b) = Right (IntVal (a-b))
operate Times (IntVal a) (IntVal b) = Right (IntVal (a*b))
operate Div (IntVal a) (IntVal b) = if b == 0 then Left "A division by zero" else Right (IntVal (a `div` b))
operate Mod (IntVal a) (IntVal b) = if b == 0 then Left "A division by zero" else Right (IntVal (a `mod` b))
operate Eq a b = if a == b then Right TrueVal else Right FalseVal
operate Less (IntVal a) (IntVal b) = if a < b then Right TrueVal else Right FalseVal
operate Greater (IntVal a) (IntVal b) = if a > b then Right TrueVal else Right FalseVal
operate In a (ListVal xs) = if a `elem` xs then Right TrueVal else Right FalseVal
operate _ _ _ = Left "Error the arguments do not satisfy the requirement."

getValue :: [Value] -> String
getValue xs = let s = concat [(valueToString x ++ " ") | x <- xs] in take ((length s) - 1) s

valueToString :: Value -> String
valueToString x = case x of 
    NoneVal -> "None"
    TrueVal -> "True"
    FalseVal -> "False"
    IntVal n -> show n
    StringVal n -> n
    ListVal xs -> let s = concat [(valueToString x) ++ ", " | x <- xs] 
        in "[" ++ (take (length s - 2) s) ++ "]"

apply :: FName -> [Value] -> Comp Value
apply "range" [] = abort (EBadArg "Empyt list")
apply "range" [IntVal v1] = return (ListVal [IntVal n| n<-[0..(v1-1)]]) 
apply "range" [IntVal v1, IntVal v2] = return (ListVal [IntVal n| n<-[v1..(v2-1)]])
apply "range" [_, _, IntVal 0] = abort (EBadArg "n3 is zero")
apply "range" [IntVal v1, IntVal v2, IntVal v3] 
    | v3 < 0 = return (ListVal[IntVal n | n <- [v1, (v1+v3)..(v2+1)]])
    | v3 > 0 = return (ListVal[IntVal n | n <- [v1, (v1+v3)..(v2-1)]])
apply "range" _ = abort (EBadArg "There are illegal values in the value list")
apply "print" xs = output (getValue xs) >> return NoneVal
apply fname _ = abort (EBadFun fname)

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval (Const v) = return v
eval (Var v) = look v
eval (Oper o e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (operate o v1 v2) of
        Right x -> return x
        Left _ ->  abort (EBadArg "this operate is illegal")
eval (Not e) = do
    v <- eval e 
    case (truthy v) of
        True -> return FalseVal
        False -> return TrueVal
eval (Call f xs) = do
  ys <- mapM eval xs
  apply f ys
eval (List xs) = do
    ys <- mapM eval xs
    return (ListVal ys)
eval (Compr e0 xs) = do
  y <- getevalcompr e0 xs
  return (ListVal y)


-- Because the output of eval compr is a Compr Value(this Value is ListVal)
-- So I cannot use do the recursive call easily because it is not easily to
-- converting [ListVal [a1], ListVal [a2],...] to ListVal [a1, a2, ...]
-- So I define a new function to do the recursive
getevalcompr :: Exp -> [CClause]-> Comp [Value]
getevalcompr e [] = sequence [eval e]
getevalcompr e (x:xs) = case x of
     CCFor v c -> do
         y <- eval c
         case y of 
             (ListVal vs) -> do
                 rs <- sequence [withBinding v n (getevalcompr e xs) | n <- vs]
                 return (concat rs)
             _ -> sequence [abort (EBadArg "The CCFor input must be ListVal")]
     CCIf c -> do
       y <- eval (Not c)
       case y of 
           TrueVal -> return []
           FalseVal -> getevalcompr e xs
           _ -> abort (EBadArg "Illegal values")

exec :: Program -> Comp ()
exec [] = return ()
exec (x:xs) = case x of
    (SDef vn exp) -> do
        y <- eval exp
        withBinding vn y (exec xs)
    (SExp exp) -> (eval exp) >> (exec xs)


execute :: Program -> ([String], Maybe RunError)
execute p = let (x, str) = (runComp (exec p) []) in case x of
    Right _ -> (str, Nothing)
    Left a -> (str, Just a)

