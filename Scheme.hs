{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}
module Scheme where

import Control.Monad.Error

data Val = IntVal Int | FloatVal Float | StringVal String | BoolVal Bool
          deriving (Eq, Ord)

instance Show Val where
  show (IntVal x) = show x
  show (FloatVal x) = show x
  show (StringVal x) = show x
  show (BoolVal x) = show x

data Exp where
  Nil :: Exp
  Val :: Val -> Exp
  If :: Exp -> Exp -> Exp -> Exp

  Plus :: Exp -> Exp -> Exp
  Minus :: Exp -> Exp -> Exp
  Times :: Exp -> Exp -> Exp
  Divide :: Exp -> Exp -> Exp

  Equal :: Exp -> Exp -> Exp
  Geq :: Exp -> Exp -> Exp
  Gt :: Exp -> Exp -> Exp
  Leq :: Exp -> Exp -> Exp
  Lt :: Exp -> Exp -> Exp

instance Show Exp where
  -- show :: Exp -> String
  show (Val x) = show x
  show Nil = "nil"
  show (Plus x y) = "(+ " ++ show x ++ " " ++ show y ++ ")"
  show (Minus x y) = "(- " ++ show x ++ " " ++ show y ++ ")"
  show (Times x y) = "(* " ++ show x ++ " " ++ show y ++ ")"
  show (Divide x y) = "(/ " ++ show x ++ " " ++ show y ++ ")"

  show (If p t e) = "(if " ++ show p ++ " " ++ show t ++ " " ++ show e ++ ")"
  show (Equal x y) = "(== " ++ show x ++ " " ++ show y ++ ")"
  show (Geq x y) = "(>= " ++ show x ++ " " ++ show y ++ ")"
  show (Gt x y) = "(> " ++ show x ++ " " ++ show y ++ ")"
  show (Leq x y) = "(<= " ++ show x ++ " " ++ show y ++ ")"
  show (Lt x y) = "(< " ++ show x ++ " " ++ show y ++ ")"

type Var = String

-- Used to build arithmetic operations
arith :: (Int -> Int -> Int) -> (Float -> Float -> Float)
      -> String -> Val -> Val -> Either String Val
arith o _  _ (IntVal x) (IntVal y) = return $ IntVal (x `o` y)
arith _ o _ (IntVal x) (FloatVal y) = return $ FloatVal ((fromIntegral x) `o` y)
arith _ o _ (FloatVal x) (IntVal y) = return $ FloatVal (x `o` (fromIntegral y))
arith _ o _ (FloatVal x) (FloatVal y) = return $ FloatVal (x `o` y)
arith _ _ s _ _ = throwError $ "Incompatible argument types in " ++ s

plus :: Val -> Val -> Either String Val
plus = arith (+) (+) "plus"

minus :: Val -> Val -> Either String Val
minus = arith (-) (-) "minus"

times :: Val -> Val -> Either String Val
times = arith (*) (*) "times"

-- Have to check div zero cases separately here
divide :: Val -> Val -> Either String Val
divide _ (IntVal 0) = throwError "Divide by zero"
divide _ (FloatVal 0) = throwError "Divide by zero"
divide x y = arith div (/) "div" x y

comp :: (Int -> Int -> Bool) -> (Float -> Float -> Bool)
     -> (Bool -> Bool -> Bool) -> (String -> String -> Bool)
     -> String -> Val -> Val -> Either String Val
comp o _ _ _ _ (IntVal x) (IntVal y) = return $ BoolVal (x `o` y)
comp _ o _ _ _ (FloatVal x) (FloatVal y) = return $ BoolVal (x `o` y)
comp _ _ o _ _ (BoolVal x) (BoolVal y) = return $ BoolVal (x `o` y)
comp _ _ _ o _ (StringVal x) (StringVal y) = return $ BoolVal (x `o` y)
comp _ _ _ _ s _ _ = throwError $ "Incompatible argument types in " ++ s

-- With equal, we don't want to throw a type error
equal :: Val -> Val -> Either String Val
equal x y = case comp (==) (==) (==) (==) "" x y of
          (Left _) -> return $ BoolVal False
          Right res -> return res

leq :: Val -> Val -> Either String Val
leq = comp (<=) (<=) (<=) (<=) "leq"

lt :: Val -> Val -> Either String Val
lt = comp (<) (<) (<) (<) "lt"

geq :: Val -> Val -> Either String Val
geq = comp (>=) (>=) (>=) (>=) "geq"

gt :: Val -> Val -> Either String Val
gt = comp (>) (>) (>) (>) "gt"

getArgs :: Exp -> ((Val -> Val -> Either String Val), Exp, Exp)
getArgs (Plus x y) = (plus, x, y)
getArgs (Minus x y) = (minus, x, y)
getArgs (Times x y) = (times, x, y)
getArgs (Divide x y) = (divide, x, y)
getArgs (Equal x y) = (equal, x, y)
getArgs (Geq x y) = (geq, x, y)
getArgs (Gt x y) = (gt, x, y)
getArgs (Leq x y) = (leq, x, y)
getArgs (Lt x y) = (lt, x, y)
getArgs _ = error "Argument handling not implemented"

eval :: Exp -> Either String Val
eval (Val x) = return x
eval Nil = throwError "Tried to eval nil"
eval (If p t e) = do
  p' <- eval p
  case p' of
    (BoolVal True) -> eval t
    (BoolVal False) -> eval e
    _ -> throwError "Incompatible argument types in if"
-- Generic binary expression handling
eval k = do
  let (op, x, y) = getArgs k
  x' <- eval x
  y' <- eval y
  op x' y'
