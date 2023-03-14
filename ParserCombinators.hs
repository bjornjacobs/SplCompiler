{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module ParserCombinators where

import Control.Applicative
import Data.Maybe
import Control.Monad
import qualified Debug.Trace

-- trace = Debug.Trace.trace
trace _ x = x

data ParserState s pos = ParserState {
    input :: [(s, pos)],
    position :: pos
} deriving (Show)

data ParserResult s pos e a = ParserResult {
    result :: Maybe a,
    state :: ParserState s pos,
    errors :: [e]
} deriving (Show)

class Empty x
instance Empty x

-- type Dbg s pos = (Empty s)
type Dbg s pos = (Show s, Show pos)

newtype Parser s pos e a = Parser {runParser :: ParserState s pos -> ParserResult s pos e a}

instance (Dbg s pos) => Functor (Parser s pos e) where
    fmap f (Parser p) = Parser $ \stt ->
        let out = p stt in
        out {result = f <$> result out}

instance (Dbg s pos) => Applicative (Parser s pos e) where
    pure a = Parser $ \stt -> ParserResult (Just a) stt []
    (Parser f) <*> (Parser p) = Parser $ \stt ->
        let (ParserResult f' stt' err1) = f stt
        in case f' of
            Nothing -> ParserResult Nothing stt' err1
            Just f' -> let (ParserResult p' stt'' err2) = trace (show stt') $ p stt'
                       in ParserResult (f' <$> p') stt'' (err1 ++ err2)

instance (Dbg s pos, Show pos) => Alternative (Parser s pos e) where
    empty = Parser $ \stt -> ParserResult Nothing stt []
    (Parser a) <|> (Parser b) = Parser $ \stt ->
        let outa = a stt
        in case result outa of
            Just _ -> outa
            _ -> b stt

instance (Dbg s pos) => Monad (Parser s pos e) where
    return = pure
    (Parser p) >>= f = Parser $ \stt ->
        let (ParserResult p' stt' err1) = p stt
        in case p' of
            Nothing -> ParserResult Nothing stt' err1
            Just p' -> let (ParserResult f' stt'' err2) = runParser (f p') stt'
                       in ParserResult f' stt'' (err1 ++ err2)

satisfy :: (Dbg s pos) => (s -> Bool) -> Parser s pos e s
satisfy f = Parser $ \stt -> case input stt of
    ((s, pos):rest) | f s -> ParserResult (Just s) (ParserState rest pos) []
             | otherwise -> ParserResult Nothing stt []
    _ -> ParserResult Nothing stt []

eof :: (Dbg s pos) => Parser s pos e ()
eof = Parser $ \stt -> case input stt of
    (_:_) -> ParserResult Nothing stt []
    _ -> ParserResult (Just ()) stt []

fail :: (Dbg s pos) => ((s, pos) -> e) -> Parser s pos e (Maybe a)
fail f = Parser $ \stt -> case input stt of
    ((s, p):rest) -> ParserResult (Just Nothing) (ParserState rest p) [f (s, p)]
    _ -> ParserResult Nothing stt []

peek :: Parser s pos e s
peek = Parser $ \stt -> case input stt of
    ((s, _):_) -> ParserResult (Just s) stt []
    _ -> ParserResult Nothing stt []




unexpect :: (Show a1, Show pos, Eq a1) => a1 -> (a1 -> pos -> e) -> Parser a1 pos e (Maybe a2)
unexpect s e = peek >>= \s' ->
    if s == s' then empty
    else Parser $ \stt -> case stt of
        (ParserState [] _) -> ParserResult Nothing stt []
        (ParserState ((s, pos):xs) _) -> ParserResult (Just Nothing) (ParserState xs pos) [e s pos]

whileNot :: (Show a1, Show pos, Eq a1) =>a1 -> Parser a1 pos e a -> (a1 -> pos -> e) -> Parser a1 pos e [a]
whileNot s p e = catMaybes <$> many (Just <$> p <|> unexpect s e)
-- whileNot s p e = catMaybes <$> impl s p e
--     where impl s p e = Parser $ \stt -> 
--             let ParserResult r stt' err = runParser p stt
--             in case r of
--                 Just r -> 
--                     let ParserResult r' stt'' err' = runParser (impl s p e) stt'
--                     in ParserResult ((Just r:) <$> r') stt'' (err ++ err')
--                 Nothing -> case input stt' of
--                     [] -> ParserResult (Just []) stt' err
--                     ((s',pos):rest) | s == s' -> ParserResult Nothing (ParserState rest pos) err
--                                     | otherwise -> ParserResult (Just []) (ParserState rest pos) (err ++ [e s' pos])

-- next :: (Eq s, Dbg s pos) => s -> Parser s pos e ()
-- next s = peek >>= \s' -> Control.Monad.unless (s' == s) empty
-- failOn :: (Dbg s pos) => ((s, pos) -> e) -> Parser s pos e b -> Parser s pos e (Maybe a, b)
-- failOn f p = Parser $ \stt -> case input stt of
--     ((s, p):rest) -> ParserResult (Just Nothing) (ParserState rest p) [f (s, p)]
--     _ -> ParserResult Nothing stt []

getPosition :: (Dbg s pos) => Parser s pos e pos
getPosition = Parser $ \stt -> ParserResult (Just $ position stt) stt []

getNextPosition :: (Dbg s pos) => Parser s pos e pos
getNextPosition = Parser $ \stt -> ParserResult (Just $ nextPosition stt) stt []

nextPosition :: ParserState s pos -> pos
nextPosition ParserState {input = (_,p):_} = p
nextPosition stt = position stt

expect :: (Dbg s pos) => Parser s pos e a -> a -> (ParserState s pos -> e) -> Parser s pos e a
expect (Parser p) a err = Parser $ \stt ->
    let out = p stt
    in case result out of
        Nothing -> out {result = Just a, errors = errors out ++ [err (state out)]}
        _ -> out
