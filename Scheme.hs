{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}

module Scheme where

import Control.Monad.Error
import Control.Monad.Reader
import Data.List(intercalate)
import Data.Map(Map)
import qualified Data.Map as M

data Val = IntVal Int | FloatVal Float | StringVal String | BoolVal Bool
          deriving (Eq, Ord)

instance Show Val where
  show (IntVal x) = show x
  show (FloatVal x) = show x
  show (StringVal x) = show x
  show (BoolVal x) = show x

-- Error/reader monad
type ER a = ErrorT String (Reader NS) a

type Var = String

type NS = Map Var Exp

data Exp where
  Exp :: [Exp] -> Exp
  
  Nil :: Exp
  Val :: Val -> Exp
  Var :: Var -> Exp
  If :: Exp -> Exp -> Exp -> Exp

  Lambda :: [Var] -> Exp -> Exp

  Plus :: Exp -> Exp -> Exp
  Minus :: Exp -> Exp -> Exp
  Times :: Exp -> Exp -> Exp
  Divide :: Exp -> Exp -> Exp

  NEqual :: Exp -> Exp -> Exp
  Equal :: Exp -> Exp -> Exp
  Geq :: Exp -> Exp -> Exp
  Gt :: Exp -> Exp -> Exp
  Leq :: Exp -> Exp -> Exp
  Lt :: Exp -> Exp -> Exp

instance Show Exp where
  show (Exp e) = "(" ++ exps ++ ")" where
    exps = intercalate " " . map show $ e
  show (Val x) = show x
  show (Var x) = x
  show Nil = "nil"
  show (Lambda vs e) = "(lambda (" ++ vars ++ ") " ++ show e ++ ")" where
    vars = intercalate " " vs

  show (Plus x y) = "(+ " ++ show x ++ " " ++ show y ++ ")"
  show (Minus x y) = "(- " ++ show x ++ " " ++ show y ++ ")"
  show (Times x y) = "(* " ++ show x ++ " " ++ show y ++ ")"
  show (Divide x y) = "(/ " ++ show x ++ " " ++ show y ++ ")"

  show (If p t e) = "(if " ++ show p ++ " " ++ show t ++ " " ++ show e ++ ")"
  show (Equal x y) = "(== " ++ show x ++ " " ++ show y ++ ")"
  show (NEqual x y) = "(!= " ++ show x ++ " " ++ show y ++ ")"
  show (Geq x y) = "(>= " ++ show x ++ " " ++ show y ++ ")"
  show (Gt x y) = "(> " ++ show x ++ " " ++ show y ++ ")"
  show (Leq x y) = "(<= " ++ show x ++ " " ++ show y ++ ")"
  show (Lt x y) = "(< " ++ show x ++ " " ++ show y ++ ")"

-- Used to build arithmetic operations
arith :: (Int -> Int -> Int) -> (Float -> Float -> Float)
      -> String -> Val -> Val -> ER Val
arith o _  _ (IntVal x) (IntVal y) = return $ IntVal (x `o` y)
arith _ o _ (IntVal x) (FloatVal y) = return $ FloatVal ((fromIntegral x) `o` y)
arith _ o _ (FloatVal x) (IntVal y) = return $ FloatVal (x `o` (fromIntegral y))
arith _ o _ (FloatVal x) (FloatVal y) = return $ FloatVal (x `o` y)
arith _ _ s _ _ = throwError $ "Incompatible argument types in " ++ s

plus :: Val -> Val -> ER Val
plus = arith (+) (+) "plus"

minus :: Val -> Val -> ER Val
minus = arith (-) (-) "minus"

times :: Val -> Val -> ER Val
times = arith (*) (*) "times"

-- Have to check div zero cases separately here
divide :: Val -> Val -> ER Val
divide _ (IntVal 0) = throwError "Divide by zero"
divide _ (FloatVal 0) = throwError "Divide by zero"
divide x y = arith div (/) "div" x y

comp :: (Int -> Int -> Bool) -> (Float -> Float -> Bool)
     -> (Bool -> Bool -> Bool) -> (String -> String -> Bool)
     -> String -> Val -> Val -> ER Val
comp o _ _ _ _ (IntVal x) (IntVal y) = return $ BoolVal (x `o` y)
comp _ o _ _ _ (FloatVal x) (FloatVal y) = return $ BoolVal (x `o` y)
comp _ _ o _ _ (BoolVal x) (BoolVal y) = return $ BoolVal (x `o` y)
comp _ _ _ o _ (StringVal x) (StringVal y) = return $ BoolVal (x `o` y)
comp _ _ _ _ s _ _ = throwError $ "Incompatible argument types in " ++ s

-- With equal, we don't want to throw a type error
equal :: Val -> Val -> ER Val
equal x y = comp (==) (==) (==) (==) "" x y
            `catchError`
            \_ -> return $ BoolVal False

nequal :: Val -> Val -> ER Val
nequal x y = comp (/=) (/=) (/=) (/=) "" x y
            `catchError`
            \_ -> return $ BoolVal True

leq :: Val -> Val -> ER Val
leq = comp (<=) (<=) (<=) (<=) "leq"

lt :: Val -> Val -> ER Val
lt = comp (<) (<) (<) (<) "lt"

geq :: Val -> Val -> ER Val
geq = comp (>=) (>=) (>=) (>=) "geq"

gt :: Val -> Val -> ER Val
gt = comp (>) (>) (>) (>) "gt"

getArgs :: Exp -> ((Val -> Val -> ER Val), Exp, Exp)
getArgs (Plus x y) = (plus, x, y)
getArgs (Minus x y) = (minus, x, y)
getArgs (Times x y) = (times, x, y)
getArgs (Divide x y) = (divide, x, y)
getArgs (Equal x y) = (equal, x, y)
getArgs (NEqual x y) = (nequal, x, y)
getArgs (Geq x y) = (geq, x, y)
getArgs (Gt x y) = (gt, x, y)
getArgs (Leq x y) = (leq, x, y)
getArgs (Lt x y) = (lt, x, y)
getArgs _ = error "Argument handling not implemented"

evalFun :: Exp -> [Exp] -> ER Val
evalFun (Lambda v e) vs = do
  vs' <- sequence $ map eval vs
  let vs'' = map Val vs'
  let varMap = M.fromList $ zip v vs''
  -- Using union with new first causes newer bindings to override older
  local (M.union varMap) (eval e)
evalFun _ _ = error "Shouldn't be calling evalFun on non-Lamba"


eval :: Exp -> ER Val
eval (Val x) = return x
eval (Var x) = do
  ns <- ask
  case M.lookup x ns of
    Nothing -> throwError $ "Undefined variable: " ++ x
    Just e -> eval e  
eval Nil = throwError "Tried to eval nil"
eval (If p t e) = do
  p' <- eval p
  case p' of
    (BoolVal True) -> eval t
    (BoolVal False) -> eval e
    _ -> throwError "Incompatible argument types in if"
eval (Lambda _ _) = throwError "Tried to eval lambda"
eval (Exp (l@(Lambda _ _):args)) = evalFun l args
-- Generic binary expression handling
eval k = do
  let (op, x, y) = getArgs k
  x' <- eval x
  y' <- eval y
  op x' y'

runEval :: Exp -> Either String Val
runEval = runEvalWithEnv M.empty

runEvalWithEnv :: NS -> Exp -> Either String Val
runEvalWithEnv env e = runReader (runErrorT (eval e)) env

testEnv :: Map String Exp
testEnv = M.fromList [("X", Val $ IntVal 0)]

testEval :: Exp -> Either String Val
testEval = runEvalWithEnv testEnv
