module Bli.Parse (
  pBool,
  pExpr,
  pNil,
  pNum,
  pOperand,
  pStr
)
where

import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (charLiteral)
import Text.Megaparsec.Char.Lexer qualified as L

import Bli.Ast

type Parser = Parsec Void Text

pNum :: Parser Literal
pNum = LitNum <$> (try float <|> integer) <?> "number literal"
  where
    float :: Parser Float
    float = lexeme L.float
    integer :: Parser Float
    integer = lexeme L.decimal

pStr :: Parser Literal
pStr = do
  strToken <- lexeme (char '"' >> manyTill charLiteral (char '"') <?> "string literal")
  let str = LitStr $ T.pack strToken
  return str

pBool :: Parser Literal
pBool =
  choice
    [ LitBool True <$ symbol "true"
    , LitBool False <$ symbol "false"
    ] <?> "boolean literal"

pNil :: Parser Literal
pNil =
  LitNil <$ symbol "nil" <?> "nil"

pOperand :: Parser Expr
pOperand =
  choice
    [ ExprLit <$> try (pNum <|> pStr <|> pBool <|> pNil)
    , ExprGroup <$> try (between (symbol "(") (symbol ")") pExpr)
    ]

pExpr :: Parser Expr
pExpr =
  makeExprParser
    pOperand
    [
      [ prefix "-" (ExprUn UnNeg)
      , prefix "!" (ExprUn UnNot)
      ]
    ,
      [ binary "*" (ExprBin BinMul)
      , binary "/" (ExprBin BinDiv)
      ]
    ,
      [ binary "+" (ExprBin BinAdd)
      , binary "-" (ExprBin BinSub)
      ]
    ,
      [ binary "<=" (ExprBin BinLte)
      , binary "<" (ExprBin BinLt)
      , binary ">=" (ExprBin BinGte)
      , binary ">" (ExprBin BinGt)
      ]
    ,
      [ binary "==" (ExprBin BinEq)
      , binary "!=" (ExprBin BinNeq)
      ]
    ,
      [ binary "and" (ExprBin BinLogAnd)
      ]
    ,
      [ binary "or" (ExprBin BinLogOr)
      ]
    ]
  where
    binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
    binary op ctor = InfixL $ ctor <$ symbol op
    prefix :: Text -> (Expr -> Expr) -> Operator Parser Expr
    prefix op ctor = Prefix $ ctor <$ symbol op

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer
