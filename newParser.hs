module Parser where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a, String)] }

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return = unit
  (>>=)  = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

-- Operations on Parsers
runParser :: Parser a -> String -> a
runParser p x = case parse p x of
                    [(v, [])] -> v
                    [(_, _)] -> error "Did not parse input completely"
                    _         -> error "Parser error"

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> case parse p s of
                        [] -> []
                        [(v, s')] -> parse (f v) s'
                        _ -> error "Looks like there was trouble"

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

option :: Parser a -> Parser a -> Parser a
option p q = Parser (\s -> case parse p s of
                              [] -> parse q s
                              res -> res)

-- some' :: Parser a -> Parser [a]
-- -- some p = Parser (\s -> case parse p s of
-- --                           [] -> []
-- --                           [(r, s')] -> r : (many p s'))
-- some' p = do v <-  p
--              vs <- many' p
--              pure (v:vs)
--
-- many' :: Parser a -> Parser [a]
-- many' p = some' p <|> pure []
-- many p = Parser (\s -> case parse p s of
--                           [] -> pure []
--                           [(r, s')]) -> : ()
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c ->
  if p c
  then unit c
  else Parser (const [])

oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s)


-- Basic Parsers
unit :: a -> Parser a
unit a = Parser $ \s -> [(a, s)]

failure :: Parser a
-- failure = Parser (\s -> [])
failure = Parser (const [])

item :: Parser Char
item = Parser $ \s -> case s of
                        [] -> []
                        (x:xs) -> [(x,xs)]

somep :: Parser (Char, Char)
somep = do { x <- item
           ; _ <- item
           ; z <- item
           ; return (x, z) }

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs)}
