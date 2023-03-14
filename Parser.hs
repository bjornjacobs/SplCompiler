{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser where
import qualified ParserCombinators
import ParserCombinators (satisfy, runParser, nextPosition, eof, fail, getNextPosition)
import CompilerPhase hiding (SyntaxError)
import qualified Error
import Control.Applicative
import Token
import AST
import Util
import Data.Foldable
import Data.Maybe
import Data.Bifunctor

type Parser a = ParserCombinators.Parser Token Position SyntaxError a

data SyntaxError
    = Expected String Position
    | Unexpected String Position
    deriving Show

class Dummy x where
    dummy :: x

instance Dummy Position where
    dummy = (0, 0)

instance Dummy () where
    dummy = ()

instance Dummy (Name Position) where
    dummy = ("x", dummy)

instance Dummy (Expr Position) where
    dummy = IntConstant 0 dummy

instance Dummy (TypeDefinition Position) where
    dummy = SimpleType "var" dummy

instance {-# OVERLAPPABLE #-} (Dummy a, Dummy b) => Dummy (a, b) where
    dummy = (dummy, dummy)

instance {-# OVERLAPPABLE #-} Dummy [a] where
    dummy = []

instance Dummy (Statement Position) where
    dummy = Return Nothing dummy

expectd :: Parser a -> a -> String -> Parser a
expectd p v str = ParserCombinators.expect p v (Expected ("Expected " ++ str) . nextPosition)

expect :: (Dummy a) => Parser a -> String -> Parser a
expect p = expectd p dummy

expects :: String -> Parser (Operator Position)
expects sym = expectd (symbol sym) (sym, dummy) ("'" ++ sym ++ "'")

expectk :: String -> Parser (String, Position)
expectk key = expectd (keyword key) (key, dummy) ("'" ++ key ++ "'")

symbol :: String -> Parser (Operator Position)
symbol str = flip (,) <$> getNextPosition <*> (getValue <$> satisfy (== OperatorToken str))

number :: Parser (Int, Position)
number = flip (,) <$> getNextPosition <*> (read . getValue <$> satisfy isNumber)

character :: Parser (Char, Position)
character = flip (,) <$> getNextPosition <*> ((\[c]->c) . getValue <$> satisfy isCharacter)

string :: Parser (String, Position)
string = flip (,) <$> getNextPosition <*> (getValue <$> satisfy isString)

keyword :: String -> Parser (String, Position)
keyword str = flip (,) <$> getNextPosition <*> (getValue <$> satisfy (== KeywordToken str) )

identifier :: Parser (Name Position)
identifier = flip (,) <$> getNextPosition <*> (getValue <$> satisfy isIdentifier)

symbolList :: [String] -> Parser (Operator Position)
symbolList = asum . map symbol

pVarDecl :: Parser (Declaration Position)
pVarDecl = (\a b c -> Variable a b c (getExtra a))
    <$> ((uncurry SimpleType <$> keyword "var") <|> pType False)
    <*> identifier
    <* expects "=" 
    <*> expect pExpr "an expression"
    <* expects ";"

pType :: Bool -> Parser (TypeDefinition Position)
pType idf = pBasicType idf
    <|> flip3 TupleType <$> (getNextPosition <* symbol "(") <*> (expect (pType idf) "a type" <* expects ",") <*> (expect (pType idf) "a type" <* expects ")")
    <|> flip ListType <$> (getNextPosition <* symbol "[") <*> (pType idf <* expects "]")

pBasicType :: Bool -> Parser (TypeDefinition Position)
pBasicType False = uncurry SimpleType <$> (keyword "Int" <|> keyword "Bool" <|> keyword "Char")
pBasicType True = uncurry SimpleType <$> (keyword "Int" <|> keyword "Bool" <|> keyword "Char" <|> identifier)

foldExpr :: Expr Position -> [(Operator Position, Expr Position)] -> Expr Position
foldExpr = foldl (\e (op, e2) -> BinOp e op e2 (getExtra e))

pExpr :: Parser (Expr Position)
pExpr = foldExpr <$> pExpr1 <*> many ((,) <$> symbol "||" <*> expect pExpr1 "an expression")
pExpr1 = foldExpr <$> pExpr2 <*> many ((,) <$> symbol "&&" <*> expect pExpr2 "an expression")
pExpr2 = makeExpr <$> pExpr3 <*> optional ((,) <$> symbolList ["==", "<", ">", "<=", ">=", "!="] <*> expect pExpr3 "an expression")
    where makeExpr expr Nothing = expr
          makeExpr expr (Just (op, e)) = BinOp expr op e (getExtra expr)
pExpr3 = foldExpr <$> pExpr4 <*> many ((,) <$> symbolList ["+", "-"] <*> expect pExpr4 "an expression")
pExpr4 = foldExpr <$> pExpr5 <*> many ((,) <$> symbolList ["*", "/", "%"] <*> expect pExpr5 "an expression")
pExpr5 = makeExpr <$> pExpr6 <*> optional ((,) <$> symbol ":" <*> expect pExpr5 "an expression")
    where makeExpr expr Nothing = expr
          makeExpr expr (Just (op, e)) = BinOp expr op e (getExtra expr)
pExpr6 = pTerm <|> (\(op, e) expr -> UnOp (op, e) expr e) <$> symbolList ["-", "!"] <*> expect pExpr6 "an expression"

list :: (Dummy a) => Parser a -> String -> Parser [a]
list p str = fromMaybe [] <$> optional ((:) <$> p <*> many (symbol "," *> expect p str))

pTerm :: Parser (Expr Position)
pTerm = symbol "(" *> expect pExpr "an expression" <* symbol ")"
    <|> (uncurry IntConstant <$> number)
    <|> (\(name, e) b -> CallExpr (name, e) b e) <$> identifier <* symbol "(" <*> list pExpr "an expression" <* expects ")"
    <|> pLValue
    <|> (uncurry CharConstant <$> character)
    <|> (uncurry BoolConstant <$> (first (const True) <$> keyword "True"))
    <|> (uncurry BoolConstant <$> (first (const False) <$> keyword "False"))
    <|> (flip3 Tuple <$> (getNextPosition <* symbol "(") <*> (expect pExpr "an expression" <* expects ",") <*> (expect pExpr "an expression" <* expects ")"))
    <|> (EmptyList <$> (snd <$> symbol "[]"))
    <|> makeList <$> string

makeList :: (String, Position) -> Expr Position 
makeList (c:cs, (row, col)) = BinOp (CharConstant c (row, col)) (":", (row, col)) (makeList (cs, (row, col + 1))) (row, col)
makeList ([], e) = EmptyList e

pIdentifier :: Parser (Expr Position)
pIdentifier = uncurry Id <$> identifier

pLValue :: Parser (Expr Position)
pLValue = foldFields <$> pIdentifier <*> many pField
    where foldFields = foldl (\e (op, e2) -> UnOp (op, e2) e e2)

pField :: Parser (String, Position)
pField = g <$> symbol "." <*> (fst <$> (keyword "hd" <|> keyword "tl" <|> keyword "snd" <|> keyword "fst"))
    where g (a, b) c = (a ++ c, b)

pStmt :: Parser (Statement Position)
pStmt = pIf <|> pWhile <|> pReturn <|> pAssignment <|> pCallStmt

pIf :: Parser (Statement Position)
pIf = (\a b c d -> If b c d (getExtra a)) <$> 
    keyword "if" <*> (expects "(" *> expect pExpr "an expression" <* expects ")")  --If
    <* expects "{" <*> whileNot "}" pStmt <* expects "}" --Then
    <*> (keyword "else" *> expects "{" *> whileNot "}" pStmt <* expects "}" <|> pure []) --else

pWhile :: Parser (Statement Position)
pWhile = flip3 While <$> (getNextPosition <* keyword "while") <*> (expects "(" *> expect pExpr "an expression" <* expects ")") --While
    <* expects "{" <*> whileNot "}" pStmt  <* expects "}" --Do


pAssignment :: Parser (Statement Position)
pAssignment = (\a b -> Assignment a b (getExtra a)) 
    <$> pLValue
    <* symbol "="
    <*> expect pExpr "an expression"
    <* expects ";"
    -- <*> pField

pCallStmt :: Parser (Statement Position)
pCallStmt = (\a b -> CallStmt a b (getExtra a)) <$> identifier <* expects "(" <*> list pExpr "an expression" <* expects ")" <* expects ";"


pReturn :: Parser (Statement Position)
pReturn = (\a b -> Return b (getExtra a)) <$> keyword "return" <*> (optional pExpr <* expects ";")

pDecl :: Parser (Declaration Position)
pDecl = pFunDecl <|> pVarDecl

pFunDecl :: Parser (Declaration Position)
pFunDecl = (\a b c d e -> Function a b c d e (getExtra a))
    <$> identifier
    <* symbol "(" 
    <*> list identifier "an identifier"
    <* expects ")"
    <*> optional (symbol "::" *> expect pFunType "a function type")
    <* expects "{"
    <*> many pVarDecl
    <*> ((:) <$> expect pStmt "a statement" -- According to the grammar you cannot have an empty function body
             <*> whileNot "}" pStmt)
    <* expects "}"

pFunType :: Parser ([TypeDefinition Position], TypeDefinition Position)
pFunType = (,) 
    <$> many (pType True)
    <* expects "->"
    <*> expect pRetType "a return type"

pRetType :: Parser (TypeDefinition Position)
pRetType = pType True <|> uncurry SimpleType <$> keyword "Void"

fail' :: Parser (Maybe a)
fail' = ParserCombinators.fail (\(s, pos) -> Unexpected ("Unexpected " ++ getValue s) pos)

whileNot :: String-> Parser a -> Parser [a]
whileNot str p = ParserCombinators.whileNot (OperatorToken str) p 
    (\s pos -> Unexpected ("Unexpected " ++ getValue s) pos)

many' p = catMaybes <$> many (Just <$> p <|> fail')

some' p = catMaybes <$> some (Just <$> p <|> fail')

parser :: Parser (Program Position)
parser = some' pDecl <* expect eof "eof"

filterErrors :: [SyntaxError] -> [Error]
filterErrors [] = []
filterErrors (x:xs) = reverse . map toError . uncurry (:) $ foldl f (x, []) xs
    where f (Unexpected msg pos, xs) (Unexpected _ _) = (Unexpected msg pos, xs)
          f (erra, xs) errb
            | getPos erra == getPos errb = (erra, xs)
            | otherwise = (errb, erra:xs)
          getPos (Unexpected _ pos) = pos
          getPos (Expected _ pos) = pos


toError :: SyntaxError -> Error
toError (Unexpected a b) = Error.SyntaxError a b
toError (Expected a b) = Error.SyntaxError a b


parseWith :: Parser a -> CompilerPhase [PosToken] a
parseWith p input = case runParser p (ParserCombinators.ParserState input (0, 0)) of
    ParserCombinators.ParserResult (Just r) _ [] -> Right r
    -- ParserCombinators.ParserResult _ _ err -> Left $ filterErrors err
    ParserCombinators.ParserResult _ _ err -> Left . map toError $ err

parseWithGuess :: Parser a -> CompilerPhase [PosToken] (ParserCombinators.ParserResult Token Position SyntaxError a )
parseWithGuess p input = Right $ runParser p (ParserCombinators.ParserState input (0, 0))
