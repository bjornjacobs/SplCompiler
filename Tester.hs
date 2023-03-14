{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Tester where

import Compiler
import Prettyprinter
import Control.Monad.State
import Control.Monad.Except
import Data.Functor
import AST
import Token
import Error
import Data.Either
import System.Directory
import System.FilePath.Posix
import Data.List
import Parser
import ParserCombinators (satisfy, runParser, nextPosition, eof, fail, getNextPosition)
import Control.Applicative
import Data.Maybe
import Data.Tuple
import CodeGeneration
import Debug.Trace
import Util
import System.Process
import CompilerPhase
import StdLib
type ErrorType = String

data Types
    = Types String Type [Types]
    deriving Show

data PhaseResult a = Skip | Error | Correct a deriving (Show)

data TestFile =
    TestFile {
        input :: String,
        parserPhase :: PhaseResult String,
        pathAnalysisPhase :: PhaseResult (),
        tarjanPhase :: PhaseResult [[String]],
        typingPhase :: PhaseResult [Types],
        overloadingPhase :: PhaseResult (),
        codeGeneratorPhase :: PhaseResult String
    } deriving (Show)

type FileReader a = StateT [String] (Either String) a
readLine :: FileReader String
readLine = do
    s <- get
    case s of
        (x:xs) -> put xs >> return x
        [] -> lift $ Left "Read empty line"

-- assert :: Bool -> FileReader ()
assert :: MonadError [Char] m => Bool -> [Char] -> m ()
assert True _ = return ()
assert False msg = throwError ("Error reading test file: " ++ msg)

readPhase :: String -> FileReader a -> FileReader (PhaseResult a)
readPhase phase reader = do
    oldState <- get
    case oldState of
        [] -> return Skip
        _ -> do
            line <- readLine
            let [p, result] = words line
            if p /= phase then do
                put oldState
                return Skip
            else do
                assert (p == phase) ("Expected " ++ phase)
                if result == "Skip" then return Skip
                else if result == "Error" then return Error
                else if result == "Correct" then Correct <$> reader
                else throwError "Incorrect result type"


readParser :: FileReader (PhaseResult String)
readParser = readPhase "#Parser" $ do
    unlines <$> readUntilEnd "EndParser"

readUntilEnd :: String -> FileReader [String]
readUntilEnd name = do
    lines <- state $ span (\str -> null str || (head str /= '#'))
    line <- readLine
    assert (line == '#':name) $ "Expected #" ++ name
    return lines


readPath :: FileReader (PhaseResult ())
readPath = readPhase "#Pathanalysis" (return ())
readTarjan :: FileReader (PhaseResult [[String]])
readTarjan = readPhase "#Tarjan" $ read <$> readLine
readTyping :: FileReader (PhaseResult [Types])
readTyping = readPhase "#Typing" $ do
    types <- unlines <$> readUntilEnd "EndTyping"
    case compileWith (many pTypes <* eof) types of
        Right types -> return types
        Left (err:_) -> lift . Left $ "Invalid type: " ++ getMessage err
readOverloading :: FileReader (PhaseResult ())
readOverloading = readPhase "#Overloading" (return ())
readCodeGen :: FileReader (PhaseResult String)
readCodeGen = readPhase "#CodeGenerator" $ do
    unlines <$> readUntilEnd "EndCodeGenerator"


end :: FileReader ()
end = do
    state <- get
    case state of
        [] -> return ()
        x:xs -> lift . Left $ "Didn't read whole file: " ++ x

readTestFile :: [String] -> Either String TestFile
readTestFile = evalStateT x
    where x = TestFile <$> (unlines <$> readUntilEnd "EndInput") <*> readParser <*> readPath <*> readTarjan <*> readTyping <*> readOverloading <*> readCodeGen <* end

-- readTestFileFile :: String -> IO (Either String TestFile)
readTestFileFile :: FilePath -> IO (Either String TestFile)
readTestFileFile name = readFile name <&> readTestFile . lines

data TestResult
      = Success
      | Fail String
      deriving (Show)


-- checkPhase :: String -> PhaseResult b -> (a -> b -> Either (Either String ()) ()) -> Either [Error.Error] a -> Either (Either String ()) ()
checkPhase phasename phase checker ast = case phase of
    Skip -> case ast of
        Right ast -> return ()
        Left _ -> Left . Left $ phasename ++ " failed, but shouldn't"
    Error -> if isLeft ast then Left (Right ()) else Left . Left $ phasename ++ " succeeded, but should fail"
    Correct result -> case ast of
        Right ast -> checker ast result
        Left _ -> Left . Left $ phasename ++ " failed, but shouldn't"

checkParser :: TestFile -> Either [Error.Error] (Program Position) -> Either (Either String ()) ()
checkParser test = checkPhase "Parsing" (parserPhase test) $ \ast result ->
        if prettyprint ast == result then Right () else Left . Left $ "Incorrect parse: " ++ diff (lines (prettyprint ast)) (lines result)

diff :: [String] -> [String] -> String
diff [] _ = ""
diff _ [] = ""
diff (x:xs) (y:ys)
    | x == y = diff xs ys
    | otherwise = show x ++ " vs " ++ show y

-- checkPathanalysis-- :: TestFile -> Either [Error.Error] (Program Position) -> Either String ()
checkPathanalysis test = checkPhase "PathAnalysis" (pathAnalysisPhase test) (\_ _ -> Right ())


checkTarjan test = checkPhase "Tarjan" (tarjanPhase test) (\parts expected ->
    if map (map getName) parts == expected then Right ()
    else Left . Left $ "Tarjan failed: " ++ show (map (map getName) parts) ++ " vs " ++ show expected
    )
    where getName (Variable _ (name, _) _ _) = name
          getName (Function (name, _) _ _ _ _ _) = name

checkTyping test = checkPhase "Typing" (typingPhase test) (\ast expected ->
    case matchTypes [] (map makeTypes ast) expected of
        Right _ -> Right ()
        Left err -> Left (Left err)
    )

checkOverloading test = checkPhase "Overloading resolution" (overloadingPhase test) (\_ _ -> Right ())
checkCodegen test = checkPhase "Code generator" (codeGeneratorPhase test) (\_ _ -> Right ())

-- doTest :: TestFile -> Either (Either String ()) ()
doTest file = do
    let parseroutput1 = compileWith parser (input file)
    let parseroutput2 = compileWith parser (input file ++ standardFunctions)
    checkParser file parseroutput1
    let pathAnalysisOutput = parseroutput1 >>= pathAnalysis
    checkPathanalysis file pathAnalysisOutput
    let tarjanOutput = pathAnalysisOutput >>= tarjan
    checkTarjan file tarjanOutput
    let typingOutput = tarjanOutput >>= typeChecker
    checkTyping file typingOutput
    let overloadingOutput = typingOutput >>= overloading
    checkOverloading file overloadingOutput
    let codegenOutput = compile (input file)
    return codegenOutput

testOutput :: TestFile -> Either a Bytecode -> IO (Either (Either String ()) ())
testOutput file (Left _) = return . Left . Left $ "code generation failed"
testOutput file (Right instructions) =
    case codeGeneratorPhase file of
        Skip -> return . Left . Right $  ()
        other -> do
                writeFile "_test.ssm" instructions
                output <- lines <$> readProcess "java" ["-jar", "ssm/ssm.jar", "--file", "_test.ssm", "--cli", "--clisteps", "500000", "--haltonerror"] ""
                matchOutput output other
        where matchOutput output Error
                | null output || last output /= "machine halted" = return . Left . Right $ ()
                | otherwise = return . Left . Left $ "The program didn't crash or timeout but should"
              matchOutput output (Correct result)
                | null output || last output /= "machine halted" =
                  return . Left . Left $ "The program timed out or crashed"
                | unlines (init output) /= result =
                    return . Left . Left $ "Incorrect output, expected " ++ result ++ ", got " ++ unlines (init output)
                | otherwise =
                    return . Left . Right $ ()
              matchOutput _ _ = return . Left . Left $ "Internal error: Skip shouldn't reach this"

runTest :: FilePath -> IO (Either (Either String ()) ())
runTest name = do
    test <- readTestFileFile (name ++ ".test")
    case test of
        Right test -> do
            let result = doTest test
            case result of
                Left x -> return (Left x)
                Right y -> testOutput test y

        Left err -> return . Left . Left $ "Incorrect test file: " ++ err

compileTest :: FilePath -> IO ()
compileTest name = do
    test <- readTestFileFile (name ++ ".test")
    case test of
        Right test -> do
            runCompiler compile (\str ->
                writeFile (name ++ ".ssm") str
                >> print "done") (input test)
        Left err -> putStrLn $ "Incorrect test file: " ++ err

getTestsFromDir :: FilePath -> IO [String]
getTestsFromDir dir = do
    files <- getDirectoryContents dir
    return $ sort $ map fst $ filter (\(_,c) -> c == ".test") $ map splitExtension files

toResult :: Either (Either String a) b -> TestResult
toResult (Left (Left x)) = Fail x
toResult (Left (Right _)) = Success
toResult (Right _) = Fail "Internal error, result should be left"

isSuccess :: TestResult -> Bool
isSuccess Success = True
isSuccess _ = False

runAndPrintTest :: FilePath -> IO TestResult
runAndPrintTest test = do
    a <- toResult <$> runTest test
    print (a, test)
    return a

runTestsWithResults :: [FilePath] -> IO Integer
runTestsWithResults dirs = do
    names <- concat <$> mapM (\dir -> map ((dir ++ "/")++) <$> getTestsFromDir dir) dirs
    results <- mapM runAndPrintTest names
    let countFails = length . filter (not . isSuccess) $ results
    putStrLn ("Tests run: " ++ show (length results) ++ ", Failed: " ++ show countFails)
    return $ toInteger countFails


pVarType :: Parser (String, Type)
pVarType = ((,) <$> (pVar <* symbol "::") <*> pDeclType) <* eof

pVar :: Parser String
pVar = fst <$> identifier

pDeclType :: Parser Type
pDeclType = (FunctionType <$> many pSimpleDeclType <*> (symbol "->" *> pSimpleDeclType))
            <|> pSimpleDeclType

instance Dummy Type where
    dummy = VoidType

pSimpleDeclType :: Parser Type
pSimpleDeclType = keyword "Int" $> IntType
          <|> keyword "Bool" $> BoolType
          <|> keyword "Char" $> CharType
          <|> keyword "Void" $> VoidType
          <|> TypeVariable . show . fst <$> number
          <|> TypeVariable . fst <$> identifier
          <|> AbstractListType <$> (symbol "[" *> expect pSimpleDeclType "a type" <* expects "]")
          <|> AbstractTupleType <$> (symbol "(" *> expect pSimpleDeclType "a type" <* expects ",") <*> (pSimpleDeclType <* expects ")")

pFunTypeDecl :: Parser Type
pFunTypeDecl = FunctionType <$> many pSimpleDeclType <* symbol "->" <*> pSimpleDeclType
    <|> pSimpleDeclType

pTypes :: Parser Types
pTypes = Types <$> pVar <* expects "::" <*> expect pFunTypeDecl "a type" <* expects ";" <*> (fromMaybe [] <$> optional moreTypes)
    where moreTypes = symbol "{" *> whileNot "}" pTypes <* expects "}"

makeTypes :: Declaration (Position, Type) -> Types
makeTypes (Variable _ (name, (_, t)) _ _) = Types name t []
makeTypes (Function (name, (_, t)) _ _ decls _ _) = Types name t $ map makeTypes decls

-- testParser :: Parser  e -> String -> Either [Error] e
-- testParser p str =  do
--     let (input, errors) = tokenize $ addPosition str
--     if not $ null errors then throwError errors
--     else do 
--         let (x, i, parseErrors) = runParser p input 0
--         if not $ null parseErrors then throwError parseErrors
--         else if null x then throwError [("Empty result", (0,0))]
--         else do
--                 let [(ast, _)] = x
--                 return ast

matchTypes :: [(String, String)] -> [Types] -> [Types] -> Either String [(String, String)]
matchTypes env a b = concat <$> mapM (`f` b) a
    where
        f t1@(Types n t ts) b = do
            t2 <- maybeToEither ("Couldn't find " ++ n) $ lookup n (map (\v@(Types m _ _) -> (m, v)) b)
            g t1 t2
        g (Types n1 t1 ts1) (Types n2 t2 ts2)
            | n1 /= n2 = Left $ n1 ++ " is not " ++ n2
            | otherwise = do
                env' <- match env t1 t2
                matchTypes env' ts1 ts2

-- Gives isomorphism between types with type variables if such an isomorphism exists
match :: [(String, String)] -> Type -> Type -> Either String [(String, String)]
match = match'
      where match' :: [(String, String)] -> Type -> Type -> Either String [(String, String)]
            match' env IntType IntType = Right env
            match' env BoolType BoolType = Right env
            match' env CharType CharType = Right env
            match' env VoidType VoidType = Right env
            match' env (AbstractTupleType a b) (AbstractTupleType c d) = join $ combine <$> match' env a c <*> match' env b d
            match' env (AbstractListType a) (AbstractListType b) = match' env a b
            match' env (FunctionType args1 ret1) (FunctionType args2 ret2) = foldl g (match' env ret1 ret2) (zip args1 args2)
            match' env (TypeVariable i) (TypeVariable j) = match'' env i j i' j'
                  where i' = lookup i env
                        j' = lookup j (map swap env)
            match' _ a b = Left $ "ACouldn't match " ++ show a ++ " with " ++ show b

            match'' :: [(String, String)] -> String -> String -> Maybe String -> Maybe String -> Either String [(String, String)]
            match'' env i j Nothing Nothing = Right ((i, j) : env)
            match'' env i j (Just i') (Just j')
                    | j == i' && j' == i = Right env
                    | otherwise = Left $ "BCouldn't match " ++ show j ++ " with " ++ show i'
            match'' env _ j (Just i') _ = Left $ "CCouldn't match " ++ j ++ " with " ++ i'
            match'' env i _ _ (Just j') = Left $ "DCouldn't match " ++ i ++ " with " ++ j'


            g :: Either String [(String, String)] -> (Type, Type) -> Either String [(String, String)]
            g (Left x) _ = Left x
            g (Right env) (a, b) = match' env a b
            combine :: [(String, String)] -> [(String, String)] -> Either String [(String, String)]
            combine [] env = Right env
            combine ((i, j):xs) env = combine xs =<< match'' env i j i' j'
                  where i' = lookup i env
                        j' = lookup j (map swap env)


