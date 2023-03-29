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

reserved :: [String]
reserved = ["set", "if", "while", "begin", "let", "letrec", "let*", "lambda"]

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
symbolName = VSym <$> name

variable :: Parser Expr
variable = EVar <$> name

int :: Parser Int
int = lexeme L.decimal

signedInt :: Parser Int
signedInt = L.signed (return ()) int

numeral :: Parser Value
numeral = VNum <$> signedInt

true :: Parser Value
true = VBool True <$ symbol "#t"

false :: Parser Value
false = VBool False <$ symbol "#f"

atom :: Parser Value
atom = symbolName <|> numeral <|> true <|> false

sexp :: Parser Value
sexp = atom <|> surround (embedList <$> many sexp)

literal :: Parser Expr
literal =
  ELiteral
    <$> (numeral <|> true <|> false <|> (char '\'' *> sexp) <|> quotedSexp)
  where
    quotedSexp = surroundBy "quote" sexp

names :: Parser [Name]
names = surround $ many name

binds :: Parser [(Name, Expr)]
binds = surround $ many (surround $ liftM2 (,) name expression)

lambdaBinds :: Parser [(Name, Expr)]
lambdaBinds = surround $ many (surround $ liftM2 (,) name parseLambda)

parseLambda :: Parser Expr
parseLambda = surroundBy "lambda" (liftM2 ELambda names expression)

expression :: Parser Expr
expression =
  try literal
    <|> try variable
    <|> try (surroundBy "if" (liftM3 EIfx expression expression expression))
    <|> try (surroundBy "begin" (EBegin <$> many expression))
    <|> try (surroundBy "let" (liftM2 (ELetx Let) binds expression))
    <|> try (surroundBy "let*" (liftM2 (ELetx LetStar) binds expression))
    <|> try (surroundBy "letrec" (liftM2 (ELetx LetRec) lambdaBinds expression))
    <|> try (surroundBy "lambda" (liftM2 ELambda names expression))
    <|> try (surround (liftM2 EApply expression (many expression)))
