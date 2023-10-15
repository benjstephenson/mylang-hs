module Symbols (InfixOperator (Plus, Minus, Asterisk, Percent, Caret)) where

data InfixOperator = Plus | Minus | Asterisk | Caret | Percent deriving (Eq, Show)
