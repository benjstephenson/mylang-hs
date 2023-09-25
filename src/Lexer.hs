module Lexer (Token (..), tokenise) where

import Data.Char (isAlpha, isNumber)
import Data.Map (Map, fromList, lookup)
import Data.Maybe (fromMaybe)

data Token
  = Number String
  | Identifier String
  | Let String
  | Equals Char
  | OpenParen Char
  | CloseParen Char
  | InfixOperator Char
  | Eof
  deriving (Eq, Show, Read)

reservedKeywords :: Map String Token
reservedKeywords = fromList [("let", Let "let")]

tokeniser :: (Char -> Bool) -> String -> (String, String)
tokeniser p chrs = _tokeniser chrs ""
 where
  _tokeniser :: String -> String -> (String, String)
  _tokeniser [] tok = ([], tok)
  _tokeniser (c : cs) tok = if (not . p) c then (c : cs, tok) else _tokeniser cs (tok ++ [c])

tokeniseAlpha :: String -> (String, String)
tokeniseAlpha = tokeniser isAlpha

tokeniseNumber :: String -> (String, String)
tokeniseNumber = tokeniser isNumber

tokenise :: String -> [Token]
tokenise [] = []
tokenise x = _tokenise x []
 where
  whitespaceChars = [' ', '\t', '\n']
  infixChars = ['+', '-', '*', '%', '/']
  _tokenise :: String -> [Token] -> [Token]
  _tokenise [] toks = toks
  _tokenise (c : cs) toks
    | c == '(' = _tokenise cs $ toks ++ [OpenParen c]
    | c == ')' = _tokenise cs $ toks ++ [CloseParen c]
    | c == ')' = _tokenise cs $ toks ++ [CloseParen c]
    | c `elem` infixChars = _tokenise cs $ toks ++ [InfixOperator c]
    | c == '=' = _tokenise cs $ toks ++ [Equals c]
    | c `elem` whitespaceChars = _tokenise cs toks
    | otherwise =
        if isNumber c
          then
            let (remainingChrs, numStr) = tokeniseNumber (c : cs)
             in _tokenise remainingChrs $ toks ++ [Number numStr]
          else
            if isAlpha c
              then
                let (remainingChrs, tok) = tokeniseAlpha $ c : cs
                    alphaToken = fromMaybe (Identifier tok) $ Data.Map.lookup tok reservedKeywords
                 in _tokenise remainingChrs $ toks ++ [alphaToken]
              else toks
