module Interp where

import Ast
import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Time.Clock
import Desugar
import Eval
import Infer
import Lens.Micro
import Lens.Micro.Extras
import Sexp
import System.Exit
import System.IO
import Text.Printf

data InputInteractivity = Prompting | NoPrompting

data OutputInteractivity = Echoing | NoEchoing

type InterpMode = (InputInteractivity, OutputInteractivity)

defaultMode :: InterpMode
defaultMode = (Prompting, Echoing)

silentMode :: InterpMode
silentMode = (NoPrompting, NoEchoing)

echoMode :: InterpMode
echoMode = (NoPrompting, Echoing)

data InterpState = InterpState
  { _evalState :: EvalState,
    _typeofState :: TypeofState
  }

initInterpState :: InterpState
initInterpState = InterpState initEvalState initTypeofState

evalState :: Lens' InterpState EvalState
evalState f (InterpState e t) = (`InterpState` t) <$> f e

typeofState :: Lens' InterpState TypeofState
typeofState f (InterpState e t) = InterpState e <$> f t

type InterpMonad = ReaderT InterpMode (StateT InterpState IO)

prompts :: InterpMode -> Bool
prompts (Prompting, _) = True
prompts (NoPrompting, _) = False

echos :: InterpMode -> Bool
echos (_, Echoing) = True
echos (_, NoEchoing) = False

runInterpMonad :: InterpMonad a -> InterpMode -> InterpState -> IO (a, InterpState)
runInterpMonad ia mode = runStateT (runReaderT ia mode)

timer :: InterpMonad a -> InterpMonad a
timer ioa = do
  t <- liftIO getCurrentTime
  a <- ioa
  t' <- liftIO getCurrentTime
  liftIO $ print (diffUTCTime t' t)
  return a

interp :: InterpMonad () -> InterpMode -> InterpState -> IO InterpState
interp ima mode s = snd <$> runInterpMonad ima mode s

interp_ :: InterpMonad () -> InterpMode -> InterpState -> IO ()
interp_ ima mode s = fst <$> runInterpMonad ima mode s

basisInterpState :: IO InterpState
basisInterpState =
  case stringToCode basis of
    Left err -> hPutStrLn stderr err >> return initInterpState
    Right codes ->
      interp (mapM_ interpCode codes) silentMode initInterpState

getEvalState :: InterpMonad EvalState
getEvalState = gets $ view Interp.evalState

putEvalState :: EvalState -> InterpMonad ()
putEvalState es = modify $ Interp.evalState .~ es

getTypeofState :: InterpMonad TypeofState
getTypeofState = gets $ view Interp.typeofState

putTypeofState :: TypeofState -> InterpMonad ()
putTypeofState ts = modify $ Interp.typeofState .~ ts

interpString :: String -> InterpMode -> InterpMonad ()
interpString string mode =
  case stringToCode string of
    Left err -> liftIO $ hPutStrLn stderr err
    Right code -> local (const mode) (mapM_ interpCode code)

interpCode :: Code -> InterpMonad ()
interpCode (Command c) = interpCommand c
interpCode (Definition d) = interpDef d

interpCommand :: Command -> InterpMonad ()
interpCommand (Use file) = do
  content <-
    liftIO $ catch
          (readFile file)
          (\e -> hPrint stderr (e :: IOException) >> return "")
  interpString content silentMode
interpCommand (Check e1 e2) = do
  let equal = DExp $ Apply (Var "=") [e1, e2]
  local (const (NoPrompting, Echoing)) (interpDef equal)

interpDef :: Ast.Def -> InterpMonad ()
interpDef def = do
  mode <- ask
  let coreDef = desugarDef def
  typeS <- getTypeofState
  let (result, typeS') = runTypeof (typeofDef coreDef) typeS
  case result of
    Left err -> liftIO $ hPutStrLn stderr err
    Right typeResponse -> do
      evalS <- getEvalState
      let (result, evalS') = runEval (evalDef coreDef) evalS
      case result of
        Left err -> liftIO $ hPutStrLn stderr err
        Right valueResponse -> do
          when (echos mode) $
            liftIO $
              if valueResponse == ""
                then mapM_ putStrLn (lines typeResponse)
                else putStrLn (printf "%s : %s" valueResponse typeResponse)
          putTypeofState typeS'
          putEvalState evalS'

repl :: InterpMonad ()
repl = forever $ do
  mode <- ask
  when
    (prompts mode)
    (liftIO $ putStr "> " >> hFlush stdout)
  eof <- liftIO isEOF
  when eof (liftIO exitSuccess)
  string <- liftIO getLine
  if ":t " `isPrefixOf` string
    then timer (interpString (drop 3 string) defaultMode)
    else interpString string defaultMode

basis :: String
basis =
  "(implicit-data bool #t #f)\
\(implicit-data unit UNIT)\
\(implicit-data ['a] list\
\               [CONS of 'a (list 'a)]\
\               NIL)\
\(implicit-data ['a] option\
\               [SOME of 'a]\
\               NONE)\
\(implicit-data ['a 'b] either\
\               [LEFT of 'a]\
\               [RIGHT of 'b])\
\(implicit-data order LESS EQUAL GREATER)\
\(implicit-data ['a 'b] pair\
\               [PAIR of 'a 'b])\
\(implicit-data ['a 'b 'c] triple\
\               [TRIPLE of 'a 'b 'c])\
\(implicit-data ['a 'b 'c 'd] 4-tuple\
\               [T4 of 'a 'b 'c 'd])\
\(implicit-data ['a 'b 'c 'd 'e] 5-tuple\
\               [T5 of 'a 'b 'c 'd 'e])\
\(implicit-data ['a 'b 'c 'd 'e 'f] 6-tuple\
\               [T6 of 'a 'b 'c 'd 'e 'f])\
\(implicit-data ['a 'b 'c 'd 'e 'f 'g] 7-tuple\
\               [T7 of 'a 'b 'c 'd 'e 'f 'g])\
\(implicit-data ['a 'b 'c 'd 'e 'f 'g 'h] 8-tuple\
\               [T8 of 'a 'b 'c 'd 'e 'f 'g 'h])\
\(implicit-data ['a 'b 'c 'd 'e 'f 'g 'h 'i]  9-tuple\
\               [T9 of 'a 'b 'c 'd 'e 'f 'g 'h 'i])\
\(implicit-data ['a 'b 'c 'd 'e 'f 'g 'h 'i 'j] 10-tuple\
\               [T10 of 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j])\
\(define*\
\    [(null? NIL) #t]\
\    [(null? (CONS _ _)) #f])\
\(define fst (p)\
\  (case p\
\    [(PAIR x _) x]))\
\(define snd (p)\
\  (case p\
\    [(PAIR _ y) y]))\
\(define Int.compare (n1 n2)\
\  (if (< n1 n2) LESS\
\      (if (< n2 n1) GREATER\
\          EQUAL)))\
\(define car (l)\
\  (case l\
\    [(CONS x xs) x]))\
\(define cdr (l)\
\  (case l\
\    [(CONS x xs) xs]))\
\(define*\
\    [(append NIL ys) ys]\
\    [(append (CONS x xs) ys) (CONS x (append xs ys))])\
\(define*\
\    [(reverse NIL) NIL]\
\    [(reverse (CONS x xs)) (append (reverse xs) (CONS x NIL))])\
\(define*\
\    [(length NIL) 0]\
\    [(length (CONS x xs)) (+ 1 (length xs))])\
\(define filter (p? xs)\
\  (case xs\
\    [NIL NIL]\
\    [(CONS y ys)\
\      (if (p? y)\
\          (CONS y (filter p? ys))\
\          (filter p? ys))]))\
\(define map (f xs)\
\  (case xs\
\    [NIL NIL]\
\    [(CONS y ys) (CONS (f y) (map f ys))]))\
\(define exists? (p? xs)\
\  (case xs\
\    [NIL #f]\
\    [(CONS y ys)\
\      (if (p? y) #t\
\          (exists? p? ys)) ]))\
\(define all? (p? xs)\
\  (case xs\
\    [NIL #t]\
\    [(CONS y ys)\
\      (if (p? y)\
\          (all? p? ys)\
\          #f)]))\
\(define foldr (op zero xs)\
\  (case xs\
\    [NIL zero]\
\    [(CONS y ys) (op y (foldr op zero ys))]))\
\(define foldl (op zero xs)\
\  (case xs\
\    [NIL zero]\
\    [(CONS y ys) (foldl op (op y zero) ys)]))\
\(define takewhile (p? xs)\
\  (case xs\
\    [NIL NIL]\
\    [(CONS y ys)\
\     (if (p? y)\
\         (CONS y (takewhile p? ys))\
\         NIL)]))\
\(define dropwhile (p? xs)\
\  (case xs\
\    [NIL NIL]\
\    [(CONS y ys)\
\     (if (p? y)\
\         (dropwhile p? ys)\
\         xs)]))\
\(define list1 (x) (CONS x NIL))\
\(define bind (x y alist)\
\  (case alist\
\    [NIL (list1 (PAIR x y))]\
\    [(CONS p ps)\
\        (if (= x (fst p))\
\            (CONS (PAIR x y) ps)\
\            (CONS p (bind x y ps)))]))\
\(define find (x alist)\
\  (case alist\
\    [NIL NONE]\
\    [(CONS (PAIR key value) pairs)\
\        (if (= x key)\
\            (SOME value)\
\            (find x pairs))]))\
\(define bound? (x alist)\
\  (case (find x alist)\
\    [(SOME _) #t]\
\    [NONE #f]))\
\(define and (b c) (if b c b))\
\(define or (b c) (if b b c))\
\(define not (b) (if b #f #t))\
\(define o (f g) (lambda (x) (f (g x))))\
\(define curry (f) (lambda (x) (lambda (y) (f x y))))\
\(define uncurry (f) (lambda (x y) ((f x) y)))\
\(define <= (x y) (not (> x y)))\
\(define >= (x y) (not (< x y)))\
\(define != (x y) (not (= x y)))\
\(define max (m n) (if (> m n) m n))\
\(define min (m n) (if (< m n) m n))\
\(define negated (n) (- 0 n))\
\(define mod (m n) (- m (* n (/ m n))))\
\(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))\
\(define lcm (m n) (* m (/ n (gcd m n))))\
\(define*\
\  [(min* NIL) NONE]\
\  [(min* (CONS x xs)) (SOME (foldr min x xs))])\
\(define*\
\  [(max* NIL) NONE]\
\  [(max* (CONS x xs)) (SOME (foldr max x xs))])"
