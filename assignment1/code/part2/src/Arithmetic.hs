-- This is a skeleton file for you to edit

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions

showExp :: Exp -> String
showExp (Cst x) = "(" ++  show x ++ ")"
showExp (Add x y) = "(" ++ showExp x ++ "+" ++ showExp y ++ ")"
showExp (Sub x y) = "(" ++ showExp x ++ "-" ++ showExp y ++ ")"
showExp (Mul x y) = "(" ++ showExp x ++ "*" ++ showExp y ++ ")"
showExp (Div x y) = "(" ++ showExp x ++ "`div`" ++ showExp y ++ ")"
showExp (Pow x y) = "(" ++ showExp x ++ "^" ++ showExp y ++ ")"
showExp _ = error "The form belongs to the commented-out part of the data Exp declaration."

evalSimple :: Exp -> Integer
evalSimple (Cst x) = x
evalSimple (Add x y) = evalSimple x + evalSimple y
evalSimple (Sub x y) = evalSimple x - evalSimple y
evalSimple (Mul x y) = evalSimple x * evalSimple y
evalSimple (Div x y)
    | evalSimple y /= 0 = evalSimple x `div` evalSimple y
    | otherwise = error "A division by Zero. "
evalSimple (Pow x y) 
    | evalSimple y < 0  = error "The second subexpression in a Pow-operation is negative."
    | evalSimple y == 0 && evalSimple x == 0  = 1
    | otherwise = evalSimple x ^ evalSimple y
evalSimple _ = error "The form belongs to the commented-out part of the data Exp declaration."

extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n e = \x -> if x == v then Just n else e x

evalFull :: Exp -> Env -> Integer  
evalFull (Cst x) _ = x
evalFull (Add x y) env = evalFull x env + evalFull y env
evalFull (Sub x y) env = evalFull x env - evalFull y env
evalFull (Mul x y) env = evalFull x env * evalFull y env
evalFull (Div x y) env
    | evalFull y env /= 0 = evalFull x env `div` evalFull y env
    | otherwise = error "A division by Zero. "
evalFull (Pow x y) env
    | evalFull y env < 0 = error "The second subexpression in a Pow-operation is negative."
    | evalFull y env == 0 && evalFull x env == 0 = 1  
    | otherwise = evalFull x env ^ evalFull y env
evalFull (If {test = c, yes = e1, no = e2}) env
    | evalFull c env /= 0 = evalFull e1 env
    | otherwise = evalFull e2 env
evalFull (Var v) env = case env v of Nothing -> error "v is not in Env"
                                     Just n -> n
evalFull (Let {var = v, def = d, body = b}) env = evalFull b (extendEnv v (evalFull d env) env)
evalFull (Sum {var = v, from = f, to = t, body = b}) env
    | evalFull f env > evalFull t env = 0
    | evalFull f env <= evalFull t env = 
      evalFull (Let v f b) env + evalFull (Sum v (Add f (Cst 1)) t b) env
evalFull _ _ = error "The form belongs to the commented-out part of the data Exp declaration."

isError :: Either ArithError Integer -> Bool
isError n = case n of
    Left _ -> True
    Right _ -> False

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst x) _ = Right x
evalErr (Div x y) e 
    | y == (Cst 0) = Left EDivZero
    | isError (evalErr x e) = evalErr x e 
    | isError (evalErr y e) = evalErr y e
    | otherwise = Right (evalFull (Div x y) e)

evalErr (Var x) e 
    | e x == Nothing = Left (EBadVar x)
    -- | isError (evalErr (Var x) e) = evalErr (Var x) e
    | otherwise = Right (evalFull (Var x) e)

evalErr (Pow x y) e 
    | isError (evalErr y e) = evalErr y e
    | (evalFull y e) < 0 = Left (ENegPower) 
    | isError (evalErr x e) = evalErr x e
    | otherwise = Right (evalFull (Pow x y) e)

evalErr (Add x y) e 
    | isError (evalErr x e) = evalErr x e 
    | isError (evalErr y e) = evalErr y e
    | otherwise = Right (evalFull (Add x y) e)

evalErr (Sub x y) e 
    | isError (evalErr x e) = evalErr x e 
    | isError (evalErr y e) = evalErr y e
    | otherwise = Right (evalFull (Sub x y) e)

evalErr (Mul x y) e  
    | isError (evalErr x e) = evalErr x e 
    | isError (evalErr y e) = evalErr y e
    | otherwise = Right (evalFull (Mul x y) e)

evalErr (Let v e1 e2) e 
    | isError (evalErr e1 e) = evalErr e1 e
    | otherwise = evalErr e2 (extendEnv v (evalFull e1 e) e)

evalErr (If e1 e2 e3) e 
    | isError (evalErr e1 e) = evalErr e1 e
    | (evalFull e1 e) /= 0 = if isError (evalErr e2 e) then evalErr e2 e 
                                                       else Right (evalFull e2 e)
    | isError (evalErr e3 e) = evalErr e3 e
    | otherwise = Right (evalFull e3 e)

evalErr (Sum v e1 e2 e3) e 
    | isError (evalErr e1 e) = evalErr e1 e
    | isError (evalErr e2 e) = evalErr e2 e
    | isError (evalErr e3 e) = evalErr e3 e
    | evalFull e1 e > evalFull e2 e = Right 0
    | isError (evalErr (Let v e1 e3) e) = evalErr (Let v e1 e3) e
    | otherwise = Right (evalFull (Sum v e1 e2 e3) e)
                                               
-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
