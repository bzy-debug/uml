module Sexp where

import Ast
import Basic
import Control.Monad.Except
import Data.Char
import Data.List
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

type ToAstMonad = Except String

runToAstMonad :: ToAstMonad e -> Either String e
runToAstMonad = runExcept

asCode :: Sexp -> ToAstMonad Code
asCode se =
  (Command <$> asCommand se)
    `catchError` const (Definition <$> asDef se)
    `catchError` const (Expression <$> asExp se)

asValue :: Sexp -> ToAstMonad Value
asValue (Atom s) =
  case (readMaybe s :: Maybe Int) of
    Just i -> return $ Num i
    Nothing -> throwError $ "Expect Number but got " ++ s
asValue (Slist (Atom "quote" : es)) =
  let vPair v vs = ConVal "cons" [v, vs]
      vNil = ConVal "'()" []
      toValue (Atom s) = maybe (Sym s) Num (readMaybe s :: Maybe Int)
      toValue (Slist es) = foldr (vPair . toValue) vNil es
   in return $ case es of
        [] -> vNil
        [se] -> toValue se
        _ -> foldr (vPair . toValue) vNil es
asValue se@(Slist _) = throwError $ "Expect Value but got " ++ show se

asCommand :: Sexp -> ToAstMonad Command
asCommand se@(Atom _) = throwError $ "Expect Command but got " ++ show se
asCommand (Slist [Atom "use", se]) = Use <$> asName se
asCommand (Slist [Atom "check", se1, se2]) = Check <$> asExp se1 <*> asValue se2
asCommand se@(Slist (Atom "use" : _)) = throwError $ "Ill formed use" ++ show se
asCommand se@(Slist (Atom "check" : _)) = throwError $ "Ill formed check" ++ show se
asCommand se = throwError $ "Expect Command but got " ++ show se

asDef :: Sexp -> ToAstMonad Def
asDef se@(Atom _) = DExp <$> asExp se
asDef (Slist [Atom "val", se1, se2]) = do
  name <- asName se1
  exp <- asExp se2
  return $ Val name exp
asDef (Slist [Atom "valrec", se1, se2]) = do
  name <- asName se1
  exp <- asExp se2
  return $ Valrec name exp
asDef (Slist [Atom "define", se1, se2, se3]) = do
  name <- asName se1
  names <- asNames se2
  exp <- asExp se3
  return $ Define name names exp
asDef se@(Slist (Atom "val" : _)) = throwError $ "Ill formed val: " ++ show se
asDef se@(Slist (Atom "valrec" : _)) = throwError $ "Ill formed valrec" ++ show se
asDef se@(Slist (Atom "define" : _)) = throwError $ "Ill formed define" ++ show se
asDef se = throwError $ "Not Define" ++ show se

asExp :: Sexp -> ToAstMonad Exp
asExp s =
  (Literal <$> asValue s)
    `catchError` \_ ->
      case s of
        Atom a ->
          return $
            if isConstructor a
              then VCon a
              else Var a
        Slist [Atom "if", se1, se2, se3] -> do
          e1 <- asExp se1
          e2 <- asExp se2
          e3 <- asExp se3
          return $ If e1 e2 e3
        Slist (Atom "begin" : ses) -> do
          es <- mapM asExp ses
          return $ Begin es
        Slist [Atom "let", ses, seBody] ->
          do
            bindings <- asBindings ses
            body <- asExp seBody
            return $ Letx Let bindings body
        -- `catchError` \_ -> do
        --   choices <- asChoices ses
        --   body <- asExp seBody
        --   return $ Letp Let choices body
        Slist [Atom "let*", ses, seBody] ->
          do
            bindings <- asBindings ses
            body <- asExp seBody
            return $ Letx LetStar bindings body
        -- `catchError` \_ -> do
        --   choices <- asChoices ses
        --   body <- asExp seBody
        --   return $ Letp LetStar choices body
        Slist [Atom "letrec", seLambdaBindings, seBody] -> do
          bindings <- asLambdaBindings seLambdaBindings
          body <- asExp seBody
          return $ Letx LetRec bindings body
        Slist [Atom "lambda", ses, seBody] ->
          do
            names <- asVariableNames ses
            body <- asExp seBody
            return $ Lambda names body
        -- `catchError` \_ -> do
        --   patterns <- asPatterns ses
        --   body <- asExp seBody
        --   return $ Lambdap patterns body
        Slist [Atom "case", se, ses] -> do
          scrutinee <- asExp se
          choices <- asChoices ses
          return $ Case scrutinee choices
        Slist (Atom "lambda" : _) -> throwError $ "Ill formed lambda: " ++ show s
        Slist (Atom "if" : _) -> throwError $ "Ill formed if:" ++ show s
        Slist (Atom "let" : _) -> throwError $ "Ill formed let:" ++ show s
        Slist (Atom "let*" : _) -> throwError $ "Ill formed let*:" ++ show s
        Slist (Atom "letrec" : _) -> throwError $ "Ill formed letrec:" ++ show s
        Slist (Atom "case" : _) -> throwError $ "Ill formed case: " ++ show s
        Slist (seRator : seRands) -> do
          rator <- asExp seRator
          rands <- mapM asExp seRands
          return $ Apply rator rands
        Slist [] -> return $ Literal (ConVal "'()" [])

isConstructor :: Name -> Bool
isConstructor s = s == "cons" || "#" `isPrefixOf` s || "make-" `isPrefixOf` s || isUpper (head s)

asConstructor :: Sexp -> ToAstMonad Name
asConstructor se = do
  name <- asName se
  if isConstructor name
    then return name
    else throwError $ "Expect Constructor Name but got " ++ show se

asVariableName :: Sexp -> ToAstMonad Name
asVariableName se = do
  name <- asName se
  if isConstructor name
    then throwError $ "Expect Variable Name but got " ++ show se
    else return name

asVariableNames :: Sexp -> ToAstMonad [Name]
asVariableNames (Slist ses) = mapM asVariableName ses
asVariableNames se = throwError $ "Expect Variable Names but got" ++ show se

asPattern :: Sexp -> ToAstMonad Pattern
asPattern (Atom s) = do
  name <- asName (Atom s)
  return $
    if s == "_"
      then Underscore
      else
        if isConstructor name
          then PApp name []
          else PVar name
asPattern (Slist [seCon, ses]) = do
  constructor <- asConstructor seCon
  patterns <- asPatterns ses
  return $ PApp constructor patterns
asPattern se = throwError $ "Expect Pattern but got " ++ show se

asPatterns :: Sexp -> ToAstMonad [Pattern]
asPatterns = undefined

asChoice :: Sexp -> ToAstMonad Choice
asChoice (Slist [sePattern, se]) = do
  pattern' <- asPattern sePattern
  exp <- asExp se
  return (pattern', exp)
asChoice se = throwError $ "Expect Choice but got " ++ show se

asChoices :: Sexp -> ToAstMonad [Choice]
asChoices (Slist ses) = mapM asChoice ses
asChoices se = throwError $ "Expect Choices but got " ++ show se

asName :: Sexp -> ToAstMonad Name
asName (Atom s) =
  case readMaybe s :: Maybe Int of
    Just i -> throwError $ "Expect Name but got Number " ++ show i
    Nothing -> return s
asName se = throwError $ "Expect Name but got " ++ show se

asNames :: Sexp -> ToAstMonad [Name]
asNames (Slist ses) = mapM asName ses
asNames se = throwError $ "Expect Names but got " ++ show se

asLambda :: Sexp -> ToAstMonad Exp
asLambda (Slist [Atom "lambda", seNames, seBody]) = do
  names <- asVariableNames seNames
  body <- asExp seBody
  return $ Lambda names body
asLambda se = throwError $ "Expect Lambda expression but got " ++ show se

asLambdaBinding :: Sexp -> ToAstMonad (Name, Exp)
asLambdaBinding (Slist [seName, seLambda]) = do
  name <- asVariableName seName
  lambdaExp <- asLambda seLambda
  return (name, lambdaExp)
asLambdaBinding se = throwError $ "Expect LambdaBinding but got " ++ show se

asLambdaBindings :: Sexp -> ToAstMonad [(Name, Exp)]
asLambdaBindings (Slist ses) = mapM asLambdaBinding ses
asLambdaBindings se = throwError $ "Expect LambdaBindings but got " ++ show se

asBinding :: Sexp -> ToAstMonad (Name, Exp)
asBinding (Slist [seName, seExp]) = do
  name <- asVariableName seName
  exp <- asExp seExp
  return (name, exp)
asBinding se = throwError $ "Expect Binding but got " ++ show se

asBindings :: Sexp -> ToAstMonad [(Name, Exp)]
asBindings (Slist es) = mapM asBinding es
asBindings se = throwError $ "Expect Bindings but got " ++ show se

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
    <|> oneOf "!$%&*/:<=?>~_^.+-@#"

name :: Parser String
name = lexeme (some letter)

parseAtom :: Parser Sexp
parseAtom = Atom <$> name

parseList :: Parser Sexp
parseList = Slist <$> surround (many parseSexp)

parseQuote :: Parser Sexp
parseQuote = do
  _ <- char '\''
  se <- parseSexp
  return $
    case se of
      Atom _ -> Slist [Atom "quote", se]
      Slist ss -> Slist (Atom "quote" : ss)

parseSexp :: Parser Sexp
parseSexp = spaceConsumer *> (parseAtom <|> parseList <|> parseQuote)

parseSexps :: Parser [Sexp]
parseSexps = many parseSexp

stringToCode :: String -> Either String [Code]
stringToCode s =
  case runParser parseSexps "stdin" s of
    Left _ -> Left "Syntax Error: Illeagal S-expression"
    Right sexps -> runToAstMonad $ mapM asCode sexps
