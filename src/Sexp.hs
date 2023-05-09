module Sexp where

import Ast
import Basic
import Control.Monad.Except
import Data.Char
import Data.List
import Data.Void
import Kind
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read
import Type

type ToAstMonad = Except String

runToAstMonad :: ToAstMonad e -> Either String e
runToAstMonad = runExcept

asCode :: Sexp -> ToAstMonad Code
asCode se =
  (Command <$> asCommand se)
    `catchError` const (Definition <$> asDef se)

asValue :: Sexp -> ToAstMonad Value
asValue (Atom s) =
  case (readMaybe s :: Maybe Int) of
    Just i -> return $ Num i
    Nothing -> throwError $ "Expect Number but got " ++ s
asValue (Slist (Atom "quote" : es)) =
  let vCon v vs = ConVal "CONS" [v, vs]
      vNil = ConVal "NIL" []
      toValue (Atom s) = maybe (Sym s) Num (readMaybe s :: Maybe Int)
      toValue (Slist es) = foldr (vCon . toValue) vNil es
   in return $ case es of
        [] -> vNil
        [se] -> toValue se
        _ -> foldr (vCon . toValue) vNil es
asValue se@(Slist _) = throwError $ "Expect Value but got " ++ show se

asCommand :: Sexp -> ToAstMonad Command
asCommand se@(Atom _) = throwError $ "Expect Command but got " ++ show se
asCommand (Slist [Atom "use", se]) = Use <$> asName se
-- asCommand (Slist [Atom "check", se1, se2]) = Check <$> asExp se1 <*> asValue se2
asCommand se@(Slist (Atom "use" : _)) = throwError $ "Ill formed use" ++ show se
-- asCommand se@(Slist (Atom "check" : _)) = throwError $ "Ill formed check" ++ show se
asCommand se = throwError $ "Expect Command but got " ++ show se

someSexp :: (Sexp -> ToAstMonad a) -> Sexp -> ToAstMonad [a]
someSexp f se@(Atom _) = mapM f [se]
someSexp f (Slist se) = mapM f se

asKind :: Sexp -> ToAstMonad Kind
asKind (Atom "*") = return Star
asKind (Slist [Atom "=>", se1, se2]) =
  liftM2 Arrow (someSexp asKind se1) (asKind se2)
asKind se = throwError $ "Expect Kind but got " ++ show se

asTypeExp :: Sexp -> ToAstMonad TypeExp
asTypeExp (Atom name)
  | isConstructor name = throwError $ "Expect type constructor but got value constructor" ++ name
  | isTypeVariable name = return $ TyVar name
  | otherwise = return $ TyCon name
asTypeExp (Slist [Atom "->", se1, se2]) = liftM2 FunTy (someSexp asTypeExp se1) (asTypeExp se2)
asTypeExp (Slist [Atom "forall", Slist se1, se2]) = liftM2 Forall (mapM asTypeVariable se1) (asTypeExp se2)
asTypeExp (Slist (se1 : se2)) = liftM2 ConApp (asTypeExp se1) (mapM asTypeExp se2)
asTypeExp se = throwError $ "Expect TypeExp but got " ++ show se

asDef :: Sexp -> ToAstMonad Def
asDef (Slist [Atom "val", se1, se2]) = do
  name <- asName se1
  exp <- asExp se2
  return $ Val name exp
asDef (Slist [Atom "valrec", se1, se2]) = do
  name <- asName se1
  exp <- asExp se2
  return $ Valrec name exp
asDef (Slist [Atom "define", se1, Slist se2, se3]) = do
  name <- asName se1
  names <- mapM asName se2
  exp <- asExp se3
  return $ Define name names exp
asDef (Slist (Atom "define*" : ses)) = do
  clauses <- mapM asClause ses
  let (names, _, _) = unzip3 clauses
  if all (head names ==) names
    then return $ DefineS clauses
    else throwError "names in clauses should be the same"
asDef (Slist (Atom "data" : se1 : se2 : se3)) = do
  kind <- asKind se1
  name <- asVariableName se2
  entries <- mapM asEntry se3
  return $ Data name kind entries
asDef (Slist (Atom "implicit-data" : (Slist se1) : se2 : se3)) = do
  typvars <- mapM asTypeVariable se1
  name <- asVariableName se2
  entries <- mapM asImplicitEntry se3
  return $ Implicit typvars name entries
asDef (Slist (Atom "implicit-data" : se1 : se2)) = do
  name <- asVariableName se1
  entries <- mapM asImplicitEntry se2
  return $ Implicit [] name entries
asDef se@(Slist (Atom "val" : _)) = throwError $ "Ill formed val: " ++ show se
asDef se@(Slist (Atom "valrec" : _)) = throwError $ "Ill formed valrec " ++ show se
asDef se@(Slist (Atom "define" : _)) = throwError $ "Ill formed define " ++ show se
asDef se@(Slist (Atom "define*" : _)) = throwError $ "Ill formed define* " ++ show se
asDef se@(Slist (Atom "data" : _)) = throwError $ "Ill formed data " ++ show se
asDef se@(Slist (Atom "implicit-data" : _)) = throwError $ "Ill formed implicit-data " ++ show se
asDef se = DExp <$> asExp se

-- TODO record

asClause :: Sexp -> ToAstMonad Clause
asClause (Slist [Slist (Atom n : ses), se]) = do
  name <- asVariableName (Atom n)
  pats <- mapM asPattern ses
  exp <- asExp se
  return (name, pats, exp)
asClause se = throwError $ "Expect a clause but got " ++ show se

asEntry :: Sexp -> ToAstMonad (Name, TypeExp)
asEntry (Slist [se1, Atom ":", se2]) = do
  cons <- asConstructor se1
  typ <- asTypeExp se2
  return (cons, typ)
asEntry se = throwError $ "Expect a data def entry but got " ++ show se

asImplicitEntry :: Sexp -> ToAstMonad (Name, [TypeExp])
asImplicitEntry (Slist (se1 : Atom "of" : se2)) = do
  cons <- asConstructor se1
  typs <- mapM asTypeExp se2
  return (cons, typs)
asImplicitEntry se1@(Atom _) = do
  cons <- asConstructor se1
  return (cons, [])
asImplicitEntry se = throwError $ "Expect a implicit data def entry but got " ++ show se

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
        Slist [Atom "let", Slist ses, seBody] -> do
          choices <- asChoices ses
          body <- asExp seBody
          return $ Letx Let choices body
        Slist [Atom "let*", Slist ses, seBody] -> do
          choices <- asChoices ses
          body <- asExp seBody
          return $ Letx LetStar choices body
        Slist [Atom "letrec", Slist seChoices, seBody] -> do
          choices <- mapM asRecChoice seChoices
          body <- asExp seBody
          return $ Letx LetRec choices body
        Slist [Atom "lambda", Slist _, _] -> asLambda s
        Slist (Atom "lambda*" : _) -> asLambdaS s
        Slist (Atom "case" : se : ses) -> do
          scrutinee <- asExp se
          choices <- asChoices ses
          return $ Case scrutinee choices
        Slist (Atom "lambda" : _) -> throwError $ "Ill formed lambda: " ++ show s
        Slist (Atom "lambda*" : _) -> throwError $ "Ill formed lambda*: " ++ show s
        Slist (Atom "if" : _) -> throwError $ "Ill formed if:" ++ show s
        Slist (Atom "let" : _) -> throwError $ "Ill formed let:" ++ show s
        Slist (Atom "let*" : _) -> throwError $ "Ill formed let*:" ++ show s
        Slist (Atom "letrec" : _) -> throwError $ "Ill formed letrec:" ++ show s
        Slist (Atom "case" : _) -> throwError $ "Ill formed case: " ++ show s
        Slist (seRator : seRands) -> do
          rator <- asExp seRator
          rands <- mapM asExp seRands
          return $ Apply rator rands
        Slist [] -> return $ Literal (ConVal "NIL" [])

asBranch :: Sexp -> ToAstMonad Branch
asBranch (Slist [Slist ses, se]) = do
  pats <- mapM asPattern ses
  exp <- asExp se
  return (pats, exp)
asBranch se = throwError $ "Expect a branch but got " ++ show se

isConstructor :: Name -> Bool
isConstructor s = "#" `isPrefixOf` s || "make-" `isPrefixOf` s || isUpper (head s)

asConstructor :: Sexp -> ToAstMonad Name
asConstructor se = do
  name <- asName se
  if isConstructor name
    then return name
    else throwError $ "Expect Constructor Name but got " ++ show se

isTypeVariable :: Name -> Bool
isTypeVariable s = "'" `isPrefixOf` s

asTypeVariable :: Sexp -> ToAstMonad Name
asTypeVariable se = do
  name <- asName se
  if isTypeVariable name
    then return name
    else throwError $ "Expect Constructor Name but got " ++ show se

asVariableName :: Sexp -> ToAstMonad Name
asVariableName se = do
  name <- asName se
  if isConstructor name
    then throwError $ "Expect Variable Name but got Constructor Name" ++ show se
    else
      if isTypeVariable name
        then throwError $ "Expect Variable Name but got Type Variable " ++ show se
        else return name

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
asPattern (Slist (seCon : ses)) = do
  constructor <- asConstructor seCon
  patterns <- mapM asPattern ses
  return $ PApp constructor patterns
asPattern se = throwError $ "Expect Pattern but got " ++ show se

asLambda :: Sexp -> ToAstMonad Exp
asLambda (Slist [Atom "lambda", Slist sePats, seBody]) = do
  patterns <- mapM asPattern sePats
  body <- asExp seBody
  return $ Lambda patterns body
asLambda se = throwError $ "Expect Lambda expression but got " ++ show se

asLambdaS :: Sexp -> ToAstMonad Exp
asLambdaS (Slist (Atom "lambda*" : ses)) = do
  branches <- mapM asBranch ses
  return $ LambdaS branches
asLambdaS se = throwError $ "Expect Lambda* expression but got " ++ show se

asRecChoice :: Sexp -> ToAstMonad Choice
asRecChoice (Slist [sePat, se]) = do
  pat <- asPattern sePat
  case pat of
    PVar _ ->
      do
        lambda <- asLambda se
        return (pat, lambda)
        `catchError` \_ -> do
          lambdaS <- asLambdaS se
          return (pat, lambdaS)
    _ -> throwError $ "letrec can only bind variable " ++ show sePat
asRecChoice se = throwError $ "Expect RecChoice but got " ++ show se

asChoice :: Sexp -> ToAstMonad Choice
asChoice (Slist [sePattern, se]) = do
  pattern' <- asPattern sePattern
  exp <- asExp se
  return (pattern', exp)
asChoice se = throwError $ "Expect Choice but got " ++ show se

asChoices :: [Sexp] -> ToAstMonad [Choice]
asChoices ses = do
  choices <- mapM asChoice ses
  let vs = concatMap (vars . fst) choices
  case duplicateName vs of
    Just x -> throwError $ x ++ " appears muptiple times in " ++ show ses
    Nothing -> return choices
  where
    vars (PVar n) = [n]
    vars (PApp _ pats) = unions (map vars pats)
    vars Underscore = []

reservedNames :: [Name]
reservedNames = ["define", "val", "valrec", "data", "implicit-data", "var", "if", "let", "let*", "letrec", "lambda", "case", "->", "=>", "forall"]

asName :: Sexp -> ToAstMonad Name
asName (Atom s) =
  case readMaybe s :: Maybe Int of
    Just i -> throwError $ "Expect Name but got Number " ++ show i
    Nothing ->
      if s `elem` reservedNames
        then throwError $ "Expect Name but got reserved name " ++ s
        else return s
asName se = throwError $ "Expect Name but got " ++ show se

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
    <|> oneOf "!$%&*/:<=?>~_^.+-@#'"

name :: Parser String
name = lexeme (some letter)

parseAtom :: Parser Sexp
parseAtom = Atom <$> name

parseList :: Parser Sexp
parseList = Slist <$> surround (many parseSexp)

parseSexp :: Parser Sexp
parseSexp = spaceConsumer *> (parseAtom <|> parseList)

parseSexps :: Parser [Sexp]
parseSexps = many parseSexp

stringToCode :: String -> Either String [Code]
stringToCode s =
  case runParser parseSexps "stdin" s of
    Left err -> Left (errorBundlePretty err)
    Right sexps -> runToAstMonad $ mapM asCode sexps
