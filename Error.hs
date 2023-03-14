{-# LANGUAGE FlexibleInstances #-}
module Error where

import Data.Either
import Util

type Position = (Int, Int) -- (row, col)

data Error
    = ErrorMessage {getMessage :: String}
    | SyntaxError {getMessage :: String, getPosition :: Position}
    | TypingError {getMessage :: String, getPosition :: Position}
    | CycleError {getMessage :: String, getPosition :: Position}
    | ReturnPathError {getMessage :: String, getPosition :: Position}
    | OverloadingResolutionError {getMessage :: String, getPosition :: Position}
    deriving (Show)

errorType :: Error -> String
errorType ErrorMessage {} = "Internal error"
errorType SyntaxError {} = "Syntax error"
errorType TypingError {} = "Type error"
errorType CycleError {} = "Cyclical dependency"
errorType ReturnPathError {} = "Return path error"
errorType OverloadingResolutionError {} = "Overloading resolution error"

instance MonadFail (Either Error) where
    fail = Left . ErrorMessage

instance MonadFail (Either [Error]) where
    fail = Left . (:[]) . ErrorMessage

collectErrors :: [Either Error a] -> Either [Error] [a]
collectErrors errors = case partitionEithers errors of
    ([], result) -> Right result
    (errors, _) -> Left errors

printError :: [String] -> Error -> IO ()
printError _ err@(ErrorMessage msg) = putStrLn (errorType err ++ ": " ++ msg)
printError input error = do
    let (row, col) = getPosition error
    let msg = getMessage error
    putStrLn (errorType error ++ ": " ++ msg ++ " at row " ++ show row ++ ", column " ++ show col)
    case input !!? (row - 1) of
        Nothing -> return ()
        Just line -> do
            putStrLn line
            putStr $ replicate (col - 1) ' '
            putStrLn "^"