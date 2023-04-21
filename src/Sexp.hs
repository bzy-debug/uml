module Sexp where

import Ast
import Control.Monad.Combinators
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read

data Sexp
  = Atom String
  | Slist [Sexp]

instance Show Sexp where
  show (Atom s) = s
  show (Slist ss) = "(" ++ unwords (map show ss) ++ ")"

type ToExpMonad = ExceptT String Identity

runToExpMonad :: ToExpMonad e -> Either String e
runToExpMonad = runIdentity . runExceptT

sexpToValue :: Sexp -> Maybe Value
sexpToValue (Atom s) = Num <$> (readMaybe s :: Maybe Int)
sexpToValue (Slist (Atom "quote" : es)) =
  let toValue (Atom s) = maybe (Sym s) Num (readMaybe s :: Maybe Int)
      toValue (Slist es) = foldr (Pair . toValue) Nil es
   in Just $ foldr (Pair . toValue) Nil es
sexpToValue (Slist _) = Nothing

sexpToExp :: Sexp -> ToExpMonad Exp
sexpToExp s =
  case sexpToValue s of
    Just v -> return $ Literal v
    Nothing ->
      case s of
        Atom a -> return $ Var a
        Slist [Atom "if", se1, se2, se3] -> do
          e1 <- sexpToExp se1
          e2 <- sexpToExp se2
          e3 <- sexpToExp se3
          return $ If e1 e2 e3
        Slist (Atom "begin" : ses) -> do
          es <- mapM sexpToExp ses
          return $ Begin es
        Slist [Atom "let", seBindings, seBody] -> do
          bindings <- asBindings seBindings
          body <- sexpToExp seBody
          return $ Letx Let bindings body
        Slist [Atom "let*", seBindings, seBody] -> do
          bindings <- asBindings seBindings
          body <- sexpToExp seBody
          return $ Letx LetStar bindings body
        Slist [Atom "letrec", seLambdaBindings, seBody] -> do
          bindings <- asLambdaBindings seLambdaBindings
          body <- sexpToExp seBody
          return $ Letx LetRec bindings body
        Slist [Atom "lambda", seNames, seBody] -> do
          names <- asNames seNames
          body <- sexpToExp seBody
          return $ Lambda names body
        Slist (Atom "lambda" : _) -> throwE $ "Ill formed lambda: " ++ show s
        Slist (Atom "if" : _) -> throwE $ "Ill formed : if" ++ show s
        Slist (Atom "let" : _) -> throwE $ "Ill formed : let" ++ show s
        Slist (Atom "let*" : _) -> throwE $ "Ill formed : let*" ++ show s
        Slist (Atom "letrec" : _) -> throwE $ "Ill formed : letrec" ++ show s
        Slist (seRator:seRands) -> do
          rator <- sexpToExp seRator
          rands <- mapM sexpToExp seRands
          return $ Apply rator rands
        Slist [] -> return $ Literal Nil

asName :: Sexp -> ToExpMonad Name
asName (Atom s) =
  case readMaybe s :: Maybe Int of
    Just i -> throwE $ "Expect Name but got Number " ++ show i 
    Nothing -> return s
asName se = throwE $ "Expect Name but got " ++ show se

asNames :: Sexp -> ToExpMonad [Name]
asNames (Slist ses) = mapM asName ses
asNames se = throwE $ "Expect Names but got " ++ show se

asLambda :: Sexp -> ToExpMonad Exp
asLambda (Slist [Atom "lambda", seNames, seBody]) = do
  names <- asNames seNames
  body <- sexpToExp seBody
  return $ Lambda names body
asLambda se = throwE $ "Expect Lambda expression but got " ++ show se

asLambdaBinding :: Sexp -> ToExpMonad (Name, Exp)
asLambdaBinding (Slist [seName, seLambda]) = do
  name <- asName seName
  lambdaExp <- asLambda seLambda
  return (name, lambdaExp)
asLambdaBinding se = throwE $ "Expect LambdaBinding but got " ++ show se

asLambdaBindings :: Sexp -> ToExpMonad [(Name, Exp)]
asLambdaBindings (Slist ses) = mapM asLambdaBinding ses
asLambdaBindings se = throwE $ "Expect LambdaBindings but got " ++ show se

asBinding :: Sexp -> ToExpMonad (Name, Exp)
asBinding (Slist [seName, seExp]) = do
  name <- asName seName
  exp <- sexpToExp seExp
  return (name, exp)
asBinding se = throwE $ "Expect Binding but got " ++ show se

asBindings :: Sexp -> ToExpMonad [(Name, Exp)]
asBindings (Slist es) = mapM asBinding es
asBindings se = throwE $ "Expect Bindings but got " ++ show se

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

paren :: Parser a -> Parser a
paren = between (symbol "(") (symbol ")")

bracket :: Parser a -> Parser a
bracket = between (symbol "[") (symbol "]")

curly :: Parser a -> Parser a
curly = between (symbol "{") (symbol "}")

surround :: Parser a -> Parser a
surround p = paren p <|> bracket p <|> curly p

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
parseSexp = spaceConsumer *> (parseAtom <|> parseList)

parseExp :: String -> Either String Exp
parseExp s =
  case runParser parseSexp "stdin" s of
    Left _ -> Left "Syntax Error: Illeagal S-expression"
    Right sexp -> runToExpMonad (sexpToExp sexp)
