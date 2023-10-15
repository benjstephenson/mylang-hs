module Main (main) where

import Parser (produceAST)

main :: IO ()
main = do
  contents <- readFile "test.txt"
  let stringLines = lines contents
  let ast = produceAST stringLines
  print ast
