{- Main.hs
 - Main entry point.
 -}

module Main where

--import System.Environment
import Language.JavaScript.Parser.Parser (readJs)
import Clang
import FSM

main :: IO ()
main = do
  indata <- getContents
--  case cprog (readJs indata) of
  case convert <$> cprog (readJs indata) of
    Just fsm -> putStrLn (show fsm)
    Nothing -> putStrLn "Error: Invalid program"
