module Token (module Token, Position) where

import Error (Position)

data Token
    = NumToken Int
    | IdToken String
    | KeywordToken String
    | OperatorToken String
    | CharToken Char
    | StringToken String
    deriving (Show, Eq)

isNumber :: Token -> Bool
isNumber (NumToken _) = True
isNumber _ = False

isString :: Token -> Bool
isString (StringToken _) = True
isString _ = False

isIdentifier :: Token -> Bool
isIdentifier (IdToken _) = True
isIdentifier _ = False

isCharacter :: Token -> Bool
isCharacter (CharToken _) = True
isCharacter _ = False

getId :: Token -> String
getId (IdToken str) = str
getId _ = ""

getValue :: Token -> String
getValue (IdToken str) = str
getValue (KeywordToken str) = str;
getValue (OperatorToken str) = str
getValue (NumToken i) = show i
getValue (CharToken c) = [c]
getValue (StringToken str) = str

getNum :: Token -> Int
getNum (NumToken val) = val
getNum _ = 0

isOperator :: Token -> Bool
isOperator (OperatorToken _) = True
isOperator _ = False

isKeyword :: Token -> Bool
isKeyword (KeywordToken _) = True
isKeyword _ = False

type PosToken = (Token, Position)
type PosString = [(Char, Position)]