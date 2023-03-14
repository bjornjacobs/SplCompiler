{-# LANGUAGE FlexibleInstances #-}
module Main (main) where

import Control.Monad.State
import System.Exit
import Data.Map (singleton)

import Analysis
import Lexer
import Util
import Parser
import ParserCombinators
import Prettyprinter
import Tester
import Typing
import Data.Array

import StdLib

import AST
import CodeGeneration

import Overloading
import System.Environment
import Compiler

main :: IO ()
main = getArg 0 >>= run

getArg :: Int -> IO String
getArg i = getArgs >>= \x -> pure $ x!!i

run :: [Char] -> IO ()
run "-c" = getArg 1 >>= \file -> compileFile file
run "-t" = runTestsWithResults ["tests/programs", "tests/unit"] >>= exitCode
                  where exitCode 0 = exitSuccess 
                        exitCode _ = exitFailure
      
