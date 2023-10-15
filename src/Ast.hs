module Ast (Expr (Program, InfixExpr, Identifier, NumericLiteral)) where

import Symbols (InfixOperator)

data Expr
  = Program [Expr]
  | InfixExpr {left :: Expr, right :: Expr, op :: InfixOperator}
  | Identifier String
  | NumericLiteral Float
  deriving (Eq, Show)
