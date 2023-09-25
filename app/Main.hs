module Main (main) where

import Lexer (Token (..), tokenise)

main :: IO ()
main = do
  contents <- readFile "test.txt"
  let stringLines = lines contents
  let tokens = tokenise `concatMap` stringLines
  print tokens
