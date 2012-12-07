module Main where

import Data.Map as M

import Text.Parsec
import System.Console.Shell
import System.Console.Shell.ShellMonad
import System.Console.Shell.Backend.Basic

import SchemeParser
import Scheme


mkShDesc :: ShellDescription NS
mkShDesc =
  initialShellDescription
       { prompt = \_ -> return "> "
       , commandStyle = OnlyCommands
       , historyEnabled = True
       }

initShState :: NS
initShState = M.empty

shell :: IO NS
shell = do
    let
      desc =
         (mkShellDescription [] shellEval)
         { greetingText       = Just ("Welcome to Scheme (sorta)!\n")
         , secondaryPrompt    = Just $ \_ -> return "] "
         }
    runShell desc basicBackend initShState

shellEval :: String -> Sh NS ()
shellEval s = case parse parseExp "" s of
    Left e -> shellPutErrLn $ show e
    Right (Define v exp) -> modifyShellSt (M.insert v exp)
    Right x -> do
      st <- getShellSt
      case runEvalWithEnv st x of
        Left e -> shellPutErrLn e
        Right p -> shellPutStrLn $ show p
  

main :: IO ()
main = do
  shell
  return ()
