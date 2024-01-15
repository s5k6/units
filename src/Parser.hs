module Parser ( parse ) where

import Data.Functor.Identity ( Identity )
import Text.Parsec hiding ( parse )
import Text.ParserCombinators.Parsec.Number ( floating2 )
import Data.RobustInt.Parsec ( bounded )
import ShuntingYard
import AST
import qualified Data.Map as M



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

uspec = given <|> omitted
  where
    omitted = pure $ Unit M.empty

    given = bracketed $ do
      above <- line
      below <- map (fmap negate) <$> (slash >> line) <|> pure []
      let a = M.unionsWith (+) $ map (uncurry M.singleton) above
      let b = M.unionsWith (+) $ map (uncurry M.singleton) below
      pure . Unit . M.filter (/=0) $ M.unionWith (+) a b
      where
        slash = lexeme $ char '/'

        line :: Parser [(String, Int)]
        line = many $ lexeme $ (,) <$> many1 letter <*> (bounded <|> pure 1)



numeral :: Parser Double

numeral = lexeme $ choice
  [ char '-' >> negate <$> floating2 True
  , floating2 True
  ]



quantity :: Parser Expression

quantity = Quantity <$> (Q <$> numeral <*> uspec)



singleton :: Parser Expression

singleton = quantity <|> parenthesised expression



expression :: Parser Expression

expression = do
  hd <- singleton
  tl <- many $ (,) <$> operator <*> singleton
  case shuntingYard (fst . table) (snd . table) Apply hd tl of
    Left conflict -> fail $ "Conflict: " ++ show conflict
    Right e -> pure e



parse :: String -> Either ParseError Expression

parse = runParser (whitespace *> expression <* eof) () ""
