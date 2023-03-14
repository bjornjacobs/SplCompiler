module Compiler (module Compiler, parser, pathAnalysis, tarjan, typeChecker, overloading, codeGenerator) where

import CompilerPhase
import Lexer
import Parser
import Error
import AST
import Analysis
import Typing
import Overloading
import CodeGeneration
import StdLib

import Prettyprinter

{-# ANN module "HLint: ignore Use <&>" #-}

type Bytecode = String

compile :: CompilerPhase String Bytecode
compile str =
    compileWith parser (str ++ standardFunctions)
    >>= pathAnalysis
    >>= tarjan
    >>= typeChecker
    >>= overloading
    >>= codeGenerator
    >>= return . unlines

compileFile :: String -> IO ()
compileFile name =
    readFile (name ++ ".spl")
    >>= runCompiler compile (\str ->
        writeFile (name ++ ".ssm") str
        >> print "done"
    )

printFile :: String -> IO ()
printFile name =
    readFile (name ++ ".spl")
    >>= runCompiler (\str -> compileWith parser str >>= pathAnalysis >>= tarjan >>= typeChecker) (\ast ->
        putStr (prettyprint ast)
        >> print "done"
    )

compileWith :: Parser a -> CompilerPhase String a
compileWith p str =
    lexer (Lexer.addPosition str)
    >>= parseWith p

-- compileWithGuess :: Parser a -> CompilerPhase String a
compileWithGuess p str =
    lexer (Lexer.addPosition str)
    >>= parseWithGuess p

