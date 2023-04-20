module Sexp where

import Control.Monad.Combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Sexp
  = Atom String
  | Slist [Sexp]
  deriving Show
-- instance Show Sexp where
--   show (Atom s) = s
--   show (Slist ss) = "(" ++ unwords (map show ss) ++ ")"

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment ";")
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

curlies :: Parser a -> Parser a
curlies = between (symbol "{") (symbol "}")

surround :: Parser a -> Parser a
surround p = parens p <|> brackets p <|> curlies p

letter :: Parser Char
letter =
  letterChar
    <|> digitChar
    <|> oneOf "!$%&*/:<=?>~_^.+-@"

name :: Parser String
name = lexeme (some letter)

parseAtom :: Parser Sexp
parseAtom = Atom <$> name

parseList :: Parser Sexp
parseList = Slist <$> surround (many parseSexp)

parseSexp :: Parser Sexp
parseSexp = spaceConsumer >> (parseAtom <|> parseList <|> (symbol "'" >> parseSexp))
