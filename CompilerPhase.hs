{-# LANGUAGE TupleSections #-}
module CompilerPhase (module Error, module CompilerPhase) where
import Error
import Control.Monad

type CompilerPhase a b = a -> Either [Error] b

combineErrors :: (a -> Either [Error] b) -> [a] -> Either [Error] [b]
combineErrors f as = foldl g (Right []) (map f as) where
    g (Left x) (Right _) = Left x
    g (Left x) (Left y) = Left (x ++ y)
    g (Right x) (Right y) = Right (x ++ [y])
    g (Right x) (Left y) = Left y

runCompiler :: CompilerPhase String b -> (b -> IO c) -> String -> IO ()
runCompiler compiler action input = case compiler input of
    Left errors -> mapM_ (printError (lines input)) errors
    Right result -> void $ action result

