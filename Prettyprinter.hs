{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

module Prettyprinter where
import AST
import Data.Typeable
import Text.Printf
import Data.List
import Lexer

data Type 
    = IntType
    | BoolType
    | CharType
    | VoidType
    | AbstractTupleType Type Type
    | AbstractListType Type
    | FunctionType [Type] Type
    | TypeVariable String
    deriving (Eq)

instance Show Type where
    show IntType = "Int"
    show BoolType = "Bool"
    show CharType = "Char"
    show VoidType = "Void"
    show (AbstractTupleType a b) = "(" ++ show a ++ ", " ++ show b ++ ")"
    show (AbstractListType a) = "[" ++ show a ++ "]"
    show (FunctionType args ret) = format args ++ " -> " ++ show ret
        where format [] = ""
              format [x] = show x
              format (x:xs) = show x ++ " " ++ format xs
    show (TypeVariable a) = a

class PrettyPrinter x where
    prettyprint :: x -> String


-- instance {-# OVERLAPS #-} (Show x) => PrettyPrinter x where
--     prettyprint x = "NOT IMPLEMENTED: " ++ show x

instance (PrettyPrinter a) => PrettyPrinter [a] where
    prettyprint = foldl (\l r -> l ++ prettyprint r) ""

instance {-# OVERLAPPABLE #-} PrettyPrinter (String, e) where
    prettyprint (str, _) = str

instance {-# OVERLAPPABLE #-} PrettyPrinter (String, (Position, Type)) where
    prettyprint (str, (_, t)) = printf "(%s :: %s)" str $ show t

instance {-# OVERLAPPABLE #-} PrettyPrinter (TypeDefinition e) where
    prettyprint (SimpleType x _) = x
    prettyprint (TupleType a b _) = printf "(%s, %s)" (prettyprint a) (prettyprint b)
    prettyprint (ListType a _) = printf "[%s]" (prettyprint a) 

instance {-# OVERLAPPABLE #-} PrettyPrinter (TypeDefinition (Position, Type)) where
    prettyprint (SimpleType x _) = x
    prettyprint (TupleType a b _) = printf "(%s, %s)" (prettyprint a) (prettyprint b)
    prettyprint (ListType a _) = printf "[%s]" (prettyprint a) 

instance {-# OVERLAPPABLE #-} PrettyPrinter (Expr e) where
    prettyprint (Id name _) = printf "%s" name -- (prettyprint field)
    prettyprint (BinOp e1 op e2 _) = printf "(%s %s %s)"  (prettyprint e1) (prettyprint op) (prettyprint e2)
    prettyprint (UnOp (op, _) e _)
        | head op == '.' = printf "%s%s" (prettyprint e) op
        | otherwise =  printf "(%s%s)" op (prettyprint e)
    prettyprint (IntConstant value _) = printf "%d" value
    prettyprint (CharConstant value _) = printf "'%s'" $ escaped value
        where escaped '\n' = "\\n"
              escaped '\t' = "\\t"
              escaped '\r' = "\\r"
              escaped '\\' = "\\\\"
              escaped c = [c]
    prettyprint (BoolConstant True _) = "True"  
    prettyprint (BoolConstant False _) = "False" 
    prettyprint (CallExpr (name, _) exprs _) = printf "%s(%s)" name (printlist ", " exprs)
    prettyprint (EmptyList _) = "[]"
    prettyprint (Tuple e1 e2 _) = printf "(%s, %s)" (prettyprint e1) (prettyprint e2) 

instance PrettyPrinter (Expr (Position, Type)) where
    prettyprint (Id name (_, t)) = printf "(%s :: %s)" name (show t) -- (prettyprint field)
    prettyprint (BinOp e1 op e2 _) = printf "(%s %s %s)"  (prettyprint e1) (prettyprint op) (prettyprint e2)
    prettyprint (UnOp (op, _) e _)
        | head op == '.' = printf "%s%s" (prettyprint e) op
        | otherwise =  printf "(%s%s)" op (prettyprint e)
    prettyprint (IntConstant value _) = printf "%d" value
    prettyprint (CharConstant value _) = printf "'%s'" $ escaped value
        where escaped '\n' = "\\n"
              escaped '\t' = "\\t"
              escaped '\r' = "\\r"
              escaped c = [c]
    prettyprint (BoolConstant True _) = "True"  
    prettyprint (BoolConstant False _) = "False" 
    prettyprint (CallExpr (name, _) exprs _) = printf "%s(%s)" name (printlist ", " exprs)
    prettyprint (EmptyList _) = "[]"
    prettyprint (Tuple e1 e2 _) = printf "(%s, %s)" (prettyprint e1) (prettyprint e2) 

-- instance PrettyPrinter (Field e) where
--     prettyprint End = ""
--     prettyprint (Head _ f) = ".hd" ++ prettyprint f
--     prettyprint (Tail _ f) = ".tl" ++ prettyprint f
--     prettyprint (First _ f) = ".fst" ++ prettyprint f
--     prettyprint (Second _ f) = ".snd" ++ prettyprint f

instance {-# INCOHERENT #-} PrettyPrinter (Statement e) where
    prettyprint (While condition statements _) 
        = "while (" ++ prettyprint condition ++ ") {\n" 
        ++ indent (prettyprint statements) ++ "}\n"
    prettyprint (If condition t e _) 
        = "if (" ++ prettyprint condition ++ ") {\n" 
        ++ indent (prettyprint t) ++ "} else {\n"
        ++ indent (prettyprint e) ++ "}\n"
    prettyprint (CallStmt name args _) 
        = prettyprint name ++ "(" ++ printlist ", " args ++ ");\n"
    prettyprint (Return Nothing _) = "return;\n"
    prettyprint (Return (Just expr) _) = "return " ++ prettyprint expr ++ ";\n"
    prettyprint (Assignment name expr _) = prettyprint name ++ " = " ++ prettyprint expr ++ ";\n"

instance {-# OVERLAPPABLE #-} PrettyPrinter (Statement (Position, Type)) where
    prettyprint (While condition statements _) 
        = "while (" ++ prettyprint condition ++ ") {\n" 
        ++ indent (prettyprint statements) ++ "}\n"
    prettyprint (If condition t e _) 
        = "if (" ++ prettyprint condition ++ ") {\n" 
        ++ indent (prettyprint t) ++ "} else {\n"
        ++ indent (prettyprint e) ++ "}\n"
    prettyprint (CallStmt name args _) 
        = prettyprint name ++ "(" ++ printlist ", " args ++ ");\n"
    prettyprint (Return Nothing _) = "return;\n"
    prettyprint (Return (Just expr) _) = "return " ++ prettyprint expr ++ ";\n"
    prettyprint (Assignment name expr _) = prettyprint name ++ " = " ++ prettyprint expr ++ ";\n"

printlist :: (PrettyPrinter a) => String -> [a] -> String
printlist sep = intercalate sep . map prettyprint

instance {-# INCOHERENT #-} PrettyPrinter (Declaration e) where 
    prettyprint (Variable t n ex _) = prettyprint t ++ " " ++ prettyprint n ++ " = " ++ prettyprint ex ++ ";\n"
    prettyprint (Function name args t declarations statements e) = prettyprint name ++ "(" ++ printlist ", " args ++ ")" ++  printtype t ++ " {\n" ++ indent (prettyprint declarations ++ prettyprint statements) ++ "}\n" 
        where printtype Nothing = ""
              printtype (Just ([], t)) = " :: -> " ++ prettyprint t
              printtype (Just (ts, t)) = " :: " ++ printlist " " ts ++ " -> " ++ prettyprint t

instance PrettyPrinter (Declaration (Position, Type)) where 
    prettyprint (Variable t n ex _) = prettyprint t ++ " " ++ prettyprint n ++ " = " ++ prettyprint ex ++ ";\n"
    prettyprint (Function name args td declarations statements (p, t)) = prettyprint name ++ "(" ++ printlist ", " args ++ ") :: " ++ show t ++ " {\n" ++ indent (prettyprint declarations ++ prettyprint statements) ++ "}\n" 
        where printtype Nothing = ""
              printtype (Just (ts, t)) = " :: " ++ printlist " " ts ++ " -> " ++ prettyprint t

indent :: String -> String
indent "" = ""
indent x = impl True x
    where impl True x = "    " ++ impl False x
          impl False ('\n':xs) = "\n" ++ indent xs
          impl False (x:xs) = x : impl False xs
          impl False "" = ""

