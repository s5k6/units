module Parser ( parse ) where

import Data.Functor.Identity ( Identity )
import Text.Parsec hiding ( parse )
import Data.RobustInt.Parsec ( bounded )
import ShuntingYard
import AST



type Parser = ParsecT String () Identity



whitespace :: Parser ()

whitespace = many (oneOf " \t\r") >> pure ()



lexeme :: Parser a -> Parser a

lexeme p = p <* whitespace



table :: Operator -> (Int, Assoc)

table o = case o of
  Add -> (6, L)
  Sub -> (6, L)
  Mul -> (7, L)
  Div -> (7, L)
  Pow -> (8, R)



operator :: Parser Operator

operator = lexeme $ choice
  [ char '+' >> pure Add
  , char '-' >> pure Sub
  , char '*' >> pure Mul
  , char '/' >> pure Div
  , char '^' >> pure Pow
  ]



parenthesised :: Parser a -> Parser a

parenthesised p = lexeme (char '(') *> p <* lexeme (char ')')



bracketed :: Parser a -> Parser a

bracketed p = lexeme (char '[') *> p <* lexeme (char ']')



uspec :: Parser Unit

uspec = bracketed given <|> one
  where
    one = pure $ Unit [] []

    given = Unit <$> line <*> ((slash >> line) <|> pure [])
      where
        slash = lexeme $ char '/'

        line :: Parser [(String, Word)]
        line = many $ lexeme $ (,) <$> many1 letter <*> (bounded <|> pure 1)



quantity :: Parser Expression

quantity = Quantity <$> lexeme bounded <*> uspec



singleton :: Parser Expression

singleton = quantity <|> parenthesised expression



expression :: Parser Expression

expression = do
  first <- singleton
  rest <- many $ (,) <$> operator <*> singleton
  case shuntingYard (fst . table) (snd . table) Apply first rest of
    Left conflict -> fail $ "Conflict: " ++ show conflict
    Right e -> pure e



parse :: String -> Either ParseError Expression

parse = runParser (whitespace *> expression <* eof) () ""
