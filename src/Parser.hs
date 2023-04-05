{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Parser where

import Ast
import Control.Monad
import Control.Monad.Combinators
import Convert
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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

surroundBy :: String -> Parser a -> Parser a
surroundBy keyword p = surround $ symbol keyword *> p

-- <letter> |!|$|%|&|*|/|:|<|=|>|?|~|_|^
letter :: Parser Char
letter =
  letterChar
    <|> oneOf "!$%&*/:<=?>~_^.+-@"

name :: Parser String
name = lexeme (some letter)

symbolName :: Parser Value
symbolName = Sym <$> name

variable :: Parser Exp
variable = Var <$> name

int :: Parser Int
int = lexeme L.decimal

signedInt :: Parser Int
signedInt = L.signed (return ()) int

numeral :: Parser Value
numeral = Num <$> signedInt

true :: Parser Value
true = Bool True <$ symbol "#t"

false :: Parser Value
false = Bool False <$ symbol "#f"

atom :: Parser Value
atom = symbolName <|> numeral <|> true <|> false

sexp :: Parser Value
sexp = atom <|> surround (embedList <$> many sexp)

literal :: Parser Exp
literal =
  Literal
    <$> (numeral <|> true <|> false <|> (char '\'' *> sexp) <|> quotedSexp)
  where
    quotedSexp = surroundBy "quote" sexp

names :: Parser [Name]
names = surround $ many name

bindings :: Parser [(Name, Exp)]
bindings = surround $ many (surround $ liftM2 (,) name expression)

expression :: Parser Exp
expression =
  try literal
    <|> try variable
    <|> try (surroundBy "if" (liftM3 If expression expression expression))
    <|> try (surroundBy "begin" (Begin <$> many expression))
    <|> try (surroundBy "let" (liftM2 (Letx Let) bindings expression))
    <|> try (surroundBy "let*" (liftM2 (Letx LetStar) bindings expression))
    <|> try (surroundBy "letrec" (liftM2 (Letx LetRec) bindings expression))
    <|> try (surroundBy "lambda" (liftM2 Lambda names expression))
    <|> try (surround (liftM2 Apply expression (many expression)))

replParse :: String -> Exp
replParse s =
  case runParser expression "stdin" s of
    Left err -> error (show err)
    Right exp -> exp
