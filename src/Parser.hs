module Parser (produceAST) where

import Ast (Expr (Identifier, InfixExpr, NumericLiteral, Program))
import Lexer (Token (..), tokenise)
import Symbols (InfixOperator (..))

type Parser = [Token] -> ([Token], Expr)

produceAST :: [String] -> Expr
produceAST rawStr = Program (_produceAST (tokenise `concatMap` rawStr) [])
 where
  _produceAST :: [Token] -> [Expr] -> [Expr]
  _produceAST [] exprs = exprs
  _produceAST (Eof : _) exprs = exprs
  _produceAST tokens exprs =
    let (remainingTokens, expr) = parseExpr tokens
     in _produceAST remainingTokens (exprs ++ [expr])

popExpected :: Token -> [Token] -> [Token]
popExpected _ [] = []
popExpected expr (t : ts) = if expr == t then ts else error ("Unexpected token type " ++ show t)

parsePrimaryExpr :: [Token] -> ([Token], Expr)
parsePrimaryExpr [] = error "Unexpected end of token list while building AST"
parsePrimaryExpr (t : ts) = case t of
  Lexer.Identifier name -> (ts, Ast.Identifier name)
  Lexer.Number value -> (ts, Ast.NumericLiteral $ read value)
  Lexer.OpenParen ->
    let (remainingTokens, expr) = parseExpr ts
     in (popExpected Lexer.CloseParen remainingTokens, expr)
  _ -> error $ "Unsupported token type " ++ show t

buildExprParser :: [InfixOperator] -> Parser -> Parser
buildExprParser infixOperators parser = parse
 where
  parse :: [Token] -> ([Token], Expr)
  parse = uncurry recurse . parser

  recurse :: [Token] -> Expr -> ([Token], Expr)
  recurse [] expr = ([], expr)
  recurse (Infix op : ts) expr
    | op `elem` infixOperators =
        let (remainingTokens, right) = parser ts
         in recurse remainingTokens (InfixExpr expr right op)
  recurse tokens expr = (tokens, expr)

parseExponentialExpr :: Parser
parseExponentialExpr = buildExprParser [Caret] parsePrimaryExpr

parseMultiplicativeExpr :: Parser
parseMultiplicativeExpr = buildExprParser [Asterisk, Percent] parseExponentialExpr

parseAdditiveExpr :: Parser
parseAdditiveExpr = buildExprParser [Plus, Minus] parseMultiplicativeExpr

parseExpr :: [Token] -> ([Token], Expr)
parseExpr = parseAdditiveExpr
