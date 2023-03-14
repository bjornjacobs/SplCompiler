{-# LANGUAGE DeriveFunctor #-}
module AST where

type Name e = (String, e)
type Operator e = (String, e)

type Program e = [Declaration e]

class AST x where
    getExtra :: x e -> e

instance AST ((,) x) where
    getExtra = snd

data TypeDefinition e
    = SimpleType String e
    | TupleType (TypeDefinition e) (TypeDefinition e) e
    | ListType (TypeDefinition e) e
    deriving (Show, Functor)

instance AST TypeDefinition where
    getExtra (SimpleType _ e) = e
    getExtra (TupleType _ _ e) = e
    getExtra (ListType _ e) = e

data Expr e
    = Id String e
    | BinOp (Expr e) (Operator e) (Expr e) e
    | UnOp (Operator e) (Expr e) e
    | IntConstant Int e
    | CharConstant Char e
    | BoolConstant Bool e
    | CallExpr (Name e) [Expr e] e
    | EmptyList e
    | Tuple (Expr e) (Expr e) e
    deriving (Show, Functor)

instance AST Expr where
    getExtra (Id _ e) = e
    getExtra (BinOp _ _ _ e) = e
    getExtra (UnOp _ _ e) = e
    getExtra (IntConstant _ e) = e
    getExtra (CharConstant _ e) = e
    getExtra (BoolConstant _ e) = e
    getExtra (CallExpr _ _ e) = e
    getExtra (EmptyList e) = e
    getExtra (Tuple _ _ e) = e

data Declaration e
    = Variable (TypeDefinition e) (Name e) (Expr e) e
    | Function (Name e) [Name e] (Maybe ([TypeDefinition e], TypeDefinition e)) [Declaration e] [Statement e] e
    deriving (Show, Functor)

instance AST Declaration where
    getExtra (Variable _ _ _ e) = e
    getExtra (Function _ _ _ _ _ e) = e

data Statement e
    = If (Expr e) [Statement e] [Statement e] e
    | While (Expr e) [Statement e] e
    | Assignment (Expr e) (Expr e) e
    | CallStmt (Name e) [Expr e] e
    | Return (Maybe (Expr e)) e
    deriving (Show, Functor)

instance AST Statement where
    getExtra (If _ _ _ e) = e
    getExtra (While _ _ e) = e
    getExtra (Assignment _ _ e) = e
    getExtra (CallStmt _ _ e) = e
    getExtra (Return _ e) = e
