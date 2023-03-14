module Lexer (module Token, lexer, addPosition) where
import Control.Applicative
import Data.Char
import Data.Maybe
import Data.Either
import Util
import CompilerPhase
import Error
import Token
    ( Position,
      PosString,
      PosToken,
      Token(..),
      isNumber,
      isString,
      isIdentifier,
      isCharacter,
      getId,
      getValue,
      getNum,
      isOperator,
      isKeyword )

addPosition :: String -> PosString
addPosition = addPosStart (1, 1)
    where addPosStart (row,col) ('\n':r) = ('\n', (row, col)) : addPosStart (row + 1, 1) r
          addPosStart (row,col) (c:r) = (c, (row, col)) : addPosStart (row, col + 1) r
          addPosStart _ [] = []

isAlphaNumUnderscore :: Char -> Bool
isAlphaNumUnderscore '_' = True
isAlphaNumUnderscore c = isAlphaNum c

keywordOrIdentifier :: String -> Token
keywordOrIdentifier str
    | str `elem` ["if", "var", "Void", "Int", "Bool", "Char", "else", "while", "return", "False", "True", "hd", "tl", "fst", "snd"] = KeywordToken str
    | otherwise = IdToken str

nextToken :: String -> (String -> Either Token String, String)
nextToken ('=':'=':_) = (Left . OperatorToken, "==")
nextToken ('[':']':_) = (Left . OperatorToken, "[]")
nextToken ('-':'>':_) = (Left . OperatorToken, "->")
nextToken ('<':'=':_) = (Left . OperatorToken, "<=")
nextToken ('>':'=':_) = (Left . OperatorToken, ">=")
nextToken ('!':'=':_) = (Left . OperatorToken, "!=")
nextToken ('&':'&':_) = (Left . OperatorToken, "&&")
nextToken ('|':'|':_) = (Left . OperatorToken, "||")
nextToken (':':':':_) = (Left . OperatorToken, "::")
nextToken('\'':cs) = (unescape, "'" ++ restOfChar cs)
    where restOfChar ('\\':c:cs) = ['\\', c, '\'']
          restOfChar (c:'\'':_) = [c, '\'']
          restOfChar (c:_) = [c]
          restOfChar _ = []
          unescape ['\'','\\', c, '\''] = Left $ CharToken $ escaped c
          unescape ['\'',c,'\''] = Left $ CharToken c
          unescape _ = Right "Invalid character token"
          escaped 'n' = '\n'
          escaped 't' = '\t'
          escaped 'r' = '\r'
          escaped c = c
nextToken('"':cs) = (swapEither . fmap StringToken . unescape, "\"" ++ restOfStr cs)
    where restOfStr ('\\':c:cs) = '\\':c : restOfStr cs
          restOfStr ('\"':_) = "\""
          restOfStr ('\n':_) = "\n"
          restOfStr (c:cs) = c : restOfStr cs
          restOfStr _ = []
          unescape ('"':str) = unescape' str
          unescape _ = Left "Invalid string token"
          unescape' ('\\':c:cs) = (escaped c :) <$> unescape' cs
          unescape' "\"" = Right ""
          unescape' ('\n':_) = Left "Expected '\"' at end of line"
          unescape' (c:cs) = (c:) <$> unescape' cs
          unescape' [] = Left "Invalid string token"
          escaped 'n' = '\n'
          escaped 't' = '\t'
          escaped 'r' = '\r'
          escaped c = c



nextToken ('/':'/':cs) = (const (Right ""), "//" ++ restOfComment cs)
    where restOfComment ('\n':_) = "\n"
          restOfComment (c:cs) = c : restOfComment cs
          restOfComment [] = []
nextToken ('/':'*':cs) = (const (Right ""), "/*" ++ restOfComment cs 1)
    where restOfComment _ 0 = []
          restOfComment [] _ = []
          restOfComment ('*':'/':cs) x = "*/" ++ restOfComment cs (x - 1)
          restOfComment ('/':'*':cs) x = "/*" ++ restOfComment cs (x + 1)
          restOfComment (c:cs) x = c : restOfComment cs x

nextToken (c:cs)
    | isDigit c = (Left . NumToken . read, takeWhile isDigit str)
    | isAlpha c = (Left . keywordOrIdentifier, takeWhile isAlphaNumUnderscore str)
    | isSpace c = (const (Right ""), [c])
    | c `elem` "=;{}(),[]+-*/%:!.<>" = (Left . OperatorToken, [c])
    | otherwise = (const (Right ""), [])
    where str = c:cs

tokenize :: PosString -> ([PosToken], [Error])
tokenize [] = ([], [])
tokenize str
    | null b = sndCons (SyntaxError ("Unrecognised character: '" ++ [char] ++ "'") pos) $ tokenize $ tail str
    | isRight tok && fromRight "" tok == "" = tokenize rest
    | isRight tok = sndCons (SyntaxError (fromRight "" tok ++ " \"" ++ b ++ "\"") pos) $ tokenize rest
    | otherwise = fstCons (fromLeft (NumToken 0) tok, pos) $ tokenize rest
    where (a, b) = nextToken $ map fst str
          tok = a b
          char = fst $ head str
          pos = snd $ head str
          rest = drop (length b) str

spanToken :: (Char -> Bool) -> ([Char] -> Token) -> PosString -> ([PosToken], [Error])
spanToken pred tok = (\(match, rest) -> fstCons (tok (map fst match), snd $ head match) (tokenize rest)) . span (pred . fst)


lexer :: CompilerPhase PosString [PosToken]
lexer input = case tokenize input of
    (output, []) -> Right output
    (_, errors) -> Left errors

