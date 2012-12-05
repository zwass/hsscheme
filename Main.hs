module Main where
import Text.Parsec
import SchemeParser
import Scheme

parseAndRun :: String -> Either String Val
parseAndRun s = case parse parseExp "" s of
  Left e -> Left $ show e
  Right p -> eval p
  
