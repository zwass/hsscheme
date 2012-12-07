{-# OPTIONS_GHC  #-}

module SchemeParser where

import Control.Applicative hiding (many, (<|>))
import Control.Monad.Error

import GHC.Float
  
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec
import Data.Functor.Identity (Identity)
  
import Scheme
  
schemeStyle   :: LanguageDef st
schemeStyle    = P.LanguageDef
                 { P.commentStart   = ""
                 , P.commentEnd     = ""
                 , P.commentLine    = ";"
                 , P.nestedComments = False
                 , P.identStart     = letter <|> char '_'
                 , P.identLetter    = alphaNum <|> oneOf "_'-"
                 , P.opStart        = P.opLetter emptyDef
                 , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
                 , P.reservedOpNames= []
                 , P.reservedNames  = ["if", "nil", "lambda", "define"]
                 , P.caseSensitive  = True
                 }

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser schemeStyle

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = P.parens lexer
identifier :: ParsecT String u Identity String
identifier = P.identifier lexer
integer :: ParsecT String u Identity Integer
integer = P.integer lexer
float :: ParsecT String u Identity Float
float = double2Float <$> P.float lexer
reserved :: String -> ParsecT String u Identity ()
reserved = P.reserved lexer
operator :: ParsecT String u Identity String
operator = P.operator lexer
symbol :: String -> ParsecT String u Identity String
symbol = P.symbol lexer
semi :: ParsecT String u Identity String
semi = P.semi lexer
semiSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
semiSep = P.semiSep lexer

parseVal :: ParsecT String u Identity Val
parseVal = try (FloatVal <$> float)
           <|> IntVal <$> integer

parseOp :: String -> (Exp -> Exp -> b) -> ParsecT String u Identity b
parseOp s o = try $ parens $ do
  reserved s
  x <- parseExp
  y <- parseExp
  return $ o x y

parseIf :: ParsecT String u Identity Exp
parseIf = try $ parens $ do
  reserved "if"
  cond <- parseExp
  t <- parseExp
  e <- parseExp
  return $ If cond t e

parseLambda :: ParsecT String u Identity Exp
parseLambda = try $ parens $ do
  reserved "lambda"
  vs <- parens (many identifier)
  e <- parseExp
  return $ Lambda vs e

parseDefine :: ParsecT String u Identity Exp
parseDefine = try $ parens $ do
  reserved "define"
  v <- identifier
  e <- parseExp
  return $ Define v e

parseExp :: ParsecT String u Identity Exp
parseExp = do { reserved "nil"; return Nil}
           <|> parseOp "+" Plus
           <|> parseOp "-" Minus
           <|> parseOp "*" Times
           <|> parseOp "/" Divide
           <|> parseOp "==" Equal
           <|> parseOp "!=" NEqual
           <|> parseOp "<" Lt
           <|> parseOp "<=" Leq
           <|> parseOp ">" Gt
           <|> parseOp ">=" Geq
           <|> parseIf
           <|> parseLambda
           <|> parseDefine
           <|> Val <$> parseVal
           <|> Var <$> identifier
           <|> Exp <$> (parens (many1 parseExp))

parseAndRun :: String -> Either String Exp
parseAndRun s = case parse parseExp "" s of
  Left e -> throwError $ show e
  Right p -> runEval p

parseAndRunWithEnv :: String -> Either String Exp
parseAndRunWithEnv s = case parse parseExp "" s of
  Left e -> throwError $ show e
  Right p -> runEvalWithEnv testEnv p
