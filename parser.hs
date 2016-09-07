module Parser where
  -- import Control.Monad
  import Data.Char

  -- class Monad a => Parser a where
  type Parser a = String -> [(a, String)]
  -- data Parser a = Parser (String -> [(a, String)])
  -- instance Monad Parser where
  --   return :: a -> Parser a
  --   return v = \inp -> [(v, inp)]
  --
  --   (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  --   p >>= f = \inp -> case parse p inp of
  --                        [] -> []
  --                        [(v, out)] -> parse (f v) out
  -- -- Basic Parsers


  returns :: a -> Parser a
  returns v = \inp -> [(v, inp)]

  failure :: Parser a
  failure = \inp -> []

  item :: Parser Char
  item = \inp -> case inp of
                     [] -> []
                     (x:xs) -> [(x, xs)]

  parse :: Parser a -> String -> [(a, String)]
  parse p inp = p inp

  (>>==) :: Parser a -> (a -> Parser b) -> Parser b
  p >>== f = \inp -> case parse p inp of
                        [] -> []
                        [(v, out)] -> parse (f v) out
  somep :: Parser (Char, Char)
  -- p = do { x <- item
  --        ; y <- item
  --        ; z <- item
  --        ; Main.return (x, z) }

  somep = item >>== \x ->
          item >>== \y ->
          item >>== \z ->
          returns (x, z)

  (+++) :: Parser a -> Parser a -> Parser a
  p +++ q = \inp -> case parse p inp of
                        [] -> parse q inp
                        [(v, out)] -> [(v, out)]

  sat :: (Char -> Bool) -> Parser Char
  sat p = item >>== \x ->
          if p x then returns x else failure

  digit :: Parser Char
  digit = sat isDigit

  lower :: Parser Char
  lower = sat isLower

  upper :: Parser Char
  upper = sat isUpper

  letter :: Parser Char
  letter = sat isAlpha

  alpahnum :: Parser Char
  alpahnum = sat isAlphaNum

  char :: Char -> Parser Char
  char x = sat (== x)

  string :: String -> Parser String
  string [] = returns []
  string (x:xs) = (char x) >>== \c ->
                  (string xs) >>== \t ->
                  returns (x:xs)

  slice :: Integer -> Parser String
  slice 0 = returns []
  slice n = item >>== \c ->
            slice (n - 1) >>== \r ->
            returns (c:r)
