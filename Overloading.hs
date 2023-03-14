{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Overloading where

import Data.Maybe
import Prettyprinter
import AST
import CompilerPhase
import Util

class ApplyOverloading x e where
    ao :: x -> Either (String, e) x                             


type Overloaded = (String, Type, String)

overloaded :: [Overloaded]
overloaded = [
    ("print", FunctionType [CharType] VoidType, "printChar"), 
    ("print", FunctionType [IntType] VoidType, "printInt"),
    ("print", FunctionType [BoolType] VoidType, "printBool"),
    ("print", FunctionType [AbstractListType CharType] VoidType, "printString"),
    ("print", FunctionType [AbstractListType IntType] VoidType, "printIntList"),
   ("==", FunctionType [IntType, IntType] BoolType, "=="),
   ("==", FunctionType [CharType, CharType] BoolType, "=="),
   ("==", FunctionType [BoolType, BoolType] BoolType, "=="),
    ("!=", FunctionType [IntType, IntType] BoolType, "!="),
    ("!=", FunctionType [CharType, CharType] BoolType, "!="),
    ("!=", FunctionType [BoolType, BoolType] BoolType, "!="),
    ("<", FunctionType [IntType, IntType] BoolType, "<"),
    ("<", FunctionType [CharType, CharType] BoolType, "<"),
    (">", FunctionType [IntType, IntType] BoolType, ">"),
    (">", FunctionType [CharType, CharType] BoolType, ">"),
    ("<=", FunctionType [IntType, IntType] BoolType, "<="),
    ("<=", FunctionType [CharType, CharType] BoolType, "<="),
    (">=", FunctionType [IntType, IntType] BoolType, ">="),
    (">=", FunctionType [CharType, CharType] BoolType, ">=")
    ]

instance ApplyOverloading (Program (e, Type)) e where
    ao = mapM ao

instance ApplyOverloading (Declaration (e, Type)) e where
    ao (Variable td n expr e) = ao expr >>= \expr' -> return $ Variable td n expr' e
    ao (Function name ns td decs stmts e) = mapM ao decs >>= \decs' -> mapM ao stmts >>= \stmts' -> return $ Function name ns td decs' stmts' e
    
instance ApplyOverloading (Statement (e, Type)) e where
    ao (If expr thens elses e) = ao expr >>= \expr' -> mapM ao thens >>= \thens' -> mapM ao elses >>= \elses' -> Right $ If expr' thens' elses' e
    ao (While expr stmts e) = ao expr >>= \expr' -> mapM ao stmts >>= \stmts' -> Right $ While expr' stmts' e
    ao (Assignment n expr e) = ao expr >>= \expr' -> Right $ Assignment n expr' e
    ao (CallStmt (name, (ne, t)) args e) = changeName name t (fst e) >>= \name' -> mapM ao args >>= \args' -> Right $ CallStmt (name', (ne, t)) args' e 
    ao (Return Nothing e) = Right $ Return Nothing e
    ao (Return (Just expr) e) = ao expr >>= \expr' -> Right $ Return (Just expr') e


instance ApplyOverloading (Expr (e, Type)) e where
    ao (CallExpr (name, (ne, t)) args e) = changeName name t ne >>= \name' -> mapM ao args >>= \args' -> Right $ CallExpr (name', (ne, t)) args' e
    ao (Tuple e1 e2 e) = ao e1 >>= \e1' -> ao e2 >>= \e2' -> Right $ Tuple e1 e2 e
    ao (BinOp e1 (op, (oe, t)) e2 e) = changeName op t oe >>= \name' -> mapM ao [e1, e2] >>= \[e1', e2'] -> 
        if name' == op then Right $ BinOp e1' (op, (oe, t)) e2' e else Right $ CallExpr (name', (oe, t)) [e1', e2'] e
    ao (UnOp op e2 e) =  ao e2 >>= \e2' -> Right $ UnOp op e2' e
    ao other = Right other
    

changeName :: String -> Type -> e -> Either (String, e) String
changeName name t p = case lookup' overloaded name t of
    (False, _) -> Right name
    (True, Just name') -> Right name'
    (True, Nothing) -> Left ("Overloaded function " ++ name ++ " not found for type: " ++ show t, p)


lookup' :: [Overloaded] -> String -> Type -> (Bool, Maybe String)
lookup' ((xn, xt, newName):xs) n t = if xn == n && xt == t then (True, Just newName) else (f || xn == n, v)
    where (f, v) = lookup' xs n t
lookup' [] n t = (False, Nothing)

overloading :: CompilerPhase (Program (Position, Type)) (Program (Position, Type))
overloading = mapLeft ((:[]) . uncurry OverloadingResolutionError) . ao
