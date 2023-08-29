module Bli.Ast where

import Data.Text (Text)
import qualified Data.Text as T

-- | The `Literal` type is used to represent
-- Loc literals: Boolean, number, string, and nil.
-- We will represent them using Haskell types:
-- Float, Text, and Bool. The nil value can be represented
-- with a nullary data constructor (a data constructor with no parameters).
-- This type should have four data constructors once completed.
data Literal
  = LitNum Float
  | LitBool Bool
  | LitStr Text
  | LitNil
  deriving(Eq, Show)
-- TODO: Finish defining the `Literal` type and remove this TODO comment

-- | The `Expr` type is used to represent all
-- of the various Lox expressions: literals, unary operations (e.g., -1),
-- binary operations (e.g., 2 * 4), and groups (e.g., (1 + 2)).
-- This type should have four data construtors once completed.
data Expr
  = ExprLit Literal 
  | ExprUn UnOp
  | ExprBin BinOp
  | ExprGroup [Expr]
  deriving (Eq, Show)

-- TODO: Finish defining the `Expr` type and remove this TODO comment

-- | There are two unary operators in Lox: 
-- arithmetic negation (e.g., -10) and logical not (e.g., !true).
-- Represent each one using a data constructor.
data UnOp
  = UnNeg
  | UnNot
  deriving (Eq, Show)

{- | There are a total of 12 binary operators in Lox.

The four arithetmic operators are: 
addition (+), subtract (-), multiply (*), and divide (/).

The six comparison and equality operators are:
less-than (<), less-than-or-equal (<=), greater-than (>), 
greater-than-or-equal (>=), equal (==), and not-equal (!=).

The two logical operators are: `and` and `or`.

Each operator should be represented by a data constructor.
-}
data BinOp
  = BinAdd
  | BinSub
  | BinMul
  | BinDiv
  | BinLt
  | BinLte
  | BinGt
  | BinGte
  | BinEq
  | BinNeq
  | BinLogAnd
  | BinLogOr
  deriving (Eq, Show)

-- TODO: Finish defining the `BinOp` type and remove this TODO comment

{- | The `stringify` function should take an `Expr`
and generate a `Text` string that resembles Lox syntax.

For example, an expression such as:
ExprUn UnNeg 
  (ExprGroup 
    (ExprBin BinAdd 
             (ExprLit LitNum 1)
             (Expr
             Lit LitNum 2.4)))
Would be represented as the string:
"-(1 + 2.4)"

Use the Lox grammar (CI 6.1) if you are unsure
what Lox code should look like.
-}
stringify :: Expr -> Text
stringify (ExprLit (LitNum x)) =
  let str = T.pack $ show x in
    -- Remove the ".0" suffix for integers
    -- E.g., the value 1.0 should be displayed as "1" and not "1.0"
    if T.isSuffixOf ".0" str then
      T.dropEnd 2 str
    else
      str
-- TODO: Finish the definiton of stringify and remove this TODO comment

-- | Returns the Lox string symbol that corresponds to the unary operator.
unOpSym :: UnOp -> Text
unOpSym op = 
  case op of
    UnNeg -> "-"
    UnNot -> "!"

-- | Returns the Lox string symbol that corresponds to the binary operator.
binOpSym :: BinOp -> Text
binOpSym op = undefined
-- TODO: Define `binOpSym` and remove this TODO comment