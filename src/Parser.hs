module Parser where

import Ast
import Control.Monad
import Control.Monad.Combinators
import Convert
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

reserved :: [T.Text]
reserved = ["set", "if", "while", "begin", "let", "letrec", "let*", "lambda"]

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment ";")
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

surround :: Parser a -> Parser a
surround p = parens p <|> brackets p

keywordSurround :: T.Text -> Parser a -> Parser a
keywordSurround keyword p = surround $ symbol keyword *> p

-- <letter> |!|$|%|&|*|/|:|<|=|>|?|~|_|^
initial :: Parser Char
initial =
  letterChar
    <|> oneOf ['!', '$', '%', '&', '*', '/', ':', '<', '>', '=', '?', '~', '_', '^']

-- <initial> | <digit 10> | . | + | - | @ |
subsequent :: Parser Char
subsequent =
  initial
    <|> digitChar
    <|> oneOf ['.', '+', '-', '@']

name :: Parser T.Text
name = lexeme $ do
  i <- initial
  s <- many subsequent
  return $ T.pack (i : s)

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
sexp = atom <|> parens (embedList <$> many sexp)

literal :: Parser Expr
literal =
  ELiteral
    <$> (numeral <|> true <|> false <|> (char '\'' *> sexp) <|> quotedSexp)
  where
    quotedSexp = keywordSurround "quote" sexp

-- set :: Parser Expr
-- set = do
--   _ <- symbol "(set"
--   var <- name
--   _ <- symbol ")"
--   ESet var <$> expression

-- ifx :: Parser Expr
-- ifx = do

lambda :: Parser Lambda
lambda = liftM2 (,) (surround $ many name) expression

binds :: Parser [(Name, Expr)]
binds = surround $ many (surround $ liftM2 (,) name expression)

expression :: Parser Expr
expression =
  try literal
    <|> try variable
    <|> try (keywordSurround "set" (liftM2 ESet name expression))
    <|> try (keywordSurround "if" (liftM3 EIfx expression expression expression))
    <|> try (keywordSurround "while" (liftM2 EWhilex expression expression))
    <|> try (keywordSurround "begin" (EBegin <$> many expression))
    <|> try (keywordSurround "let" (liftM2 (ELetx Let) binds expression))
    <|> try (keywordSurround "let*" (liftM2 (ELetx LetStar) binds expression))
    <|> try (keywordSurround "letrec" (liftM2 (ELetx LetRec) binds expression))
    <|> try (keywordSurround "lambda" (ELambda <$> lambda))
    <|> try (parens (liftM2 EApply expression (many expression)))
