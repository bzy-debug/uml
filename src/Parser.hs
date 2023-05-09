module Parser where

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f (Parser pa) = Parser $
    \s ->
      case pa s of
        Nothing -> Nothing
        Just (a, s') -> Just (f a, s')

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (<*>) (Parser pf) (Parser pa) = Parser $
    \s ->
      case pf s of
        Nothing -> Nothing
        Just (f, s1) ->
          case pa s1 of
            Nothing -> Nothing
            Just (a, s2) -> Just (f a, s2)

instance Monad Parser where
  (>>=) (Parser pa) f = Parser $
    \s ->
      case pa s of
        Nothing -> Nothing
        Just (a, s1) -> runParser (f a) s1
