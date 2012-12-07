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

initShState = M.empty

shell = do
    let
      desc =
         (mkShellDescription [] shellEval)
         { greetingText       = Just ("Hello, shell")
         , secondaryPrompt    = Just $ \_ -> return "] "
         }
    runShell desc basicBackend initShState

shellEval :: String -> Sh NS ()
shellEval s =
  case parseAndRun s of
    Left e -> shellPutErrLn e
    Right p -> shellPutStrLn $ show p
  
