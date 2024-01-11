module Main ( main ) where

import Parser
import AST ( eval )


main :: IO ()

main = do

  input <- head . lines <$> getContents

  case parse input of

    Left m -> do
      putStr "Error: "
      print m

    Right e -> do
      putStr "AST: "
      print e
      putStr "Value: "
      print $ eval e
