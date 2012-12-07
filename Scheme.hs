{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}

module Scheme where

import Control.Monad.Error
import Control.Monad.Reader
import Data.List(intercalate)
import Data.Map(Map)
import qualified Data.Map as M

data Val = IntVal Integer| FloatVal Float | StringVal String | BoolVal Bool
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

  Define :: Var -> Exp -> Exp
  
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
--  deriving Show -- (Useful for debugging)

instance Show Exp where
  show (Exp e) = "(" ++ exps ++ ")" where
    exps = intercalate " " . map show $ e
  show (Define v e) = "(define " ++ v ++ " " ++ show e ++ ")"
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
arith :: (Integer-> Integer-> Integer) -> (Float -> Float -> Float)
      -> String -> Exp -> Exp -> ER Exp
arith o _  _ (Val (IntVal x)) (Val (IntVal y)) =
  return . Val $ IntVal (x `o` y)
arith _ o _ (Val (IntVal x)) (Val (FloatVal y)) =
  return . Val $ FloatVal ((fromIntegral x) `o` y)
arith _ o _ (Val (FloatVal x)) (Val (IntVal y)) =
  return . Val $ FloatVal (x `o` (fromIntegral y))
arith _ o _ (Val (FloatVal x)) (Val (FloatVal y)) =
  return . Val $ FloatVal (x `o` y)
arith _ _ s _ _ = throwError $ "Incompatible argument types in " ++ s

plus :: Exp -> Exp -> ER Exp
plus = arith (+) (+) "plus"

minus :: Exp -> Exp -> ER Exp
minus = arith (-) (-) "minus"

times :: Exp -> Exp -> ER Exp
times = arith (*) (*) "times"

-- Have to check div zero cases separately here
divide :: Exp -> Exp -> ER Exp
divide _ (Val (IntVal 0)) = throwError "Divide by zero"
divide _ (Val (FloatVal 0)) = throwError "Divide by zero"
divide x y = arith div (/) "div" x y

comp :: (Integer-> Integer-> Bool) -> (Float -> Float -> Bool)
     -> (Bool -> Bool -> Bool) -> (String -> String -> Bool)
     -> String -> Exp -> Exp -> ER Exp
comp o _ _ _ _ (Val (IntVal x)) (Val (IntVal y)) =
  return . Val $ BoolVal (x `o` y)
comp _ o _ _ _ (Val (FloatVal x)) (Val (FloatVal y)) =
  return . Val $ BoolVal (x `o` y)
comp _ _ o _ _ (Val (BoolVal x)) (Val (BoolVal y)) =
  return . Val $ BoolVal (x `o` y)
comp _ _ _ o _ (Val (StringVal x)) (Val (StringVal y)) =
  return . Val $ BoolVal (x `o` y)
comp _ _ _ _ s _ _ = throwError $ "Incompatible argument types in " ++ s

-- With equal, we don't want to throw a type error
equal :: Exp -> Exp -> ER Exp
equal x y = comp (==) (==) (==) (==) "" x y
            `catchError`
            \_ -> return . Val $ BoolVal False

nequal :: Exp -> Exp -> ER Exp
nequal x y = comp (/=) (/=) (/=) (/=) "" x y
            `catchError`
            \_ -> return . Val $ BoolVal True

leq :: Exp -> Exp -> ER Exp
leq = comp (<=) (<=) (<=) (<=) "leq"

lt :: Exp -> Exp -> ER Exp
lt = comp (<) (<) (<) (<) "lt"

geq :: Exp -> Exp -> ER Exp
geq = comp (>=) (>=) (>=) (>=) "geq"

gt :: Exp -> Exp -> ER Exp
gt = comp (>) (>) (>) (>) "gt"

getArgs :: Exp -> ((Exp -> Exp -> ER Exp), Exp, Exp)
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
getArgs x = error $ "Argument handling not implemented:\n" ++ show x

evalFun :: Exp -> [Exp] -> ER Exp
evalFun l@(Lambda v e) vs
  | length v == length vs = do
    vs' <- sequence . (map eval) $ vs
    let varMap = M.fromList $ zip v vs'
    -- Using union with new first causes newer bindings to override older
    local (M.union varMap) (eval e)
  | otherwise = throwError $ "The function " ++ show l
                ++ " has been called with " ++ show (length vs)
                ++ " arguments, but requires exactly " ++ show (length v)
                ++ " arguments."
evalFun e _ = throwError $ show e ++ " is not applicable"

lookupVar :: Var -> ER Exp
lookupVar v = do
  ns <- ask
  case M.lookup v ns of
    Nothing -> throwError $ "Undefined variable: " ++ v
    Just e -> return e

eval :: Exp -> ER Exp
eval v@(Val _) = return v
eval l@(Lambda _ _) = return l
eval (Var x) = do
  e <- lookupVar x
  eval e
eval Nil = return Nil
eval (If p t e) = do
  p' <- eval p
  case p' of
    (Val (BoolVal True)) -> eval t
    (Val (BoolVal False)) -> eval e
    _ -> throwError "Incompatible argument types in if"
eval (Exp ((Var x):args)) = do
  l <- lookupVar x
  evalFun l args
eval (Exp (l:args)) = evalFun l args
eval k = do
  let (op, x, y) = getArgs k
  x' <- eval x
  y' <- eval y
  op x' y'

runEval :: Exp -> Either String Exp
runEval = runEvalWithEnv M.empty

runEvalWithEnv :: NS -> Exp -> Either String Exp
runEvalWithEnv env e = runReader (runErrorT (eval e)) env

testEnv :: Map String Exp
testEnv = M.fromList [("X", Val $ IntVal 0)]

testEval :: Exp -> Either String Exp
testEval = runEvalWithEnv testEnv
