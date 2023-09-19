module Bli.Ast where

import Data.Text (Text)
import qualified Data.Text as T

-- | The `Literal` type is used to represent
-- Lox literals: Boolean, number, string, and nil.
-- We will represent them using Haskell types:
-- Float, Text, and Bool. The nil value can be represented
-- with an empty data constructor.
-- This type should have four data constructors once completed.
data Literal
  = LitNum Float
  | LitStr Text
  | LitBool Bool
  | LitNil
  deriving (Eq, Show)

-- | The `Expr` type is used to represent all
-- of the various Lox expressions: literals, unary operations (e.g., -1),
-- binary operations (e.g., 2 * 4), and groups (e.g., (1 + 2)).
-- This type should have four data construtors once completed.
data Expr
  = ExprLit Literal
  | ExprUn UnOp Expr
  | ExprBin BinOp Expr Expr
  | ExprGroup Expr
  deriving (Eq, Show)

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
  = BinEq
  | BinNeq
  | BinLt
  | BinLte
  | BinGt
  | BinGte
  | BinAdd
  | BinSub
  | BinMul
  | BinDiv
  | BinLogAnd
  | BinLogOr
  deriving (Eq, Show)

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
stringify (ExprLit LitNil) = "nil"
stringify (ExprLit (LitNum x)) =
  let str = T.pack $ show x in
    if T.isSuffixOf ".0" str then
      T.dropEnd 2 str
    else
      str
stringify (ExprLit (LitBool x)) =
  if x then "true" else "false"
stringify (ExprLit (LitStr x)) = "\"" <> x <> "\""
stringify (ExprGroup x) = "(" <> stringify x <> ")"
stringify (ExprUn op x) = unOpSym op <> stringify x
stringify (ExprBin op x y) = stringify x <> " " <> binOpSym op <> " " <> stringify y

-- | Returns the Lox string symbol that corresponds to the unary operator.
unOpSym :: UnOp -> Text
unOpSym op = 
  case op of
    UnNeg -> "-"
    UnNot -> "!"

-- | Returns the Lox string symbol that corresponds to the binary operator.
binOpSym :: BinOp -> Text
binOpSym op =
  case op of
    BinAdd -> "+"
    BinSub -> "-"
    BinMul -> "*"
    BinDiv -> "/"
    BinLt -> "<"
    BinLte -> "<="
    BinGt -> ">"
    BinGte -> ">="
    BinEq -> "=="
    BinNeq -> "!="
    BinLogAnd -> "and"
    BinLogOr -> "or"

newtype ErrorMsg = ErrorMsg Text
  deriving (Eq, Show)

mkErrorMsg :: Show a => a -> a -> ErrorMsg
mkErrorMsg found expected =
  ErrorMsg . T.pack $ "Found " <> show found <> ", but expected " <> show expected <> "."

-- | Recursively evaluate an expression.
eval :: Expr -> Either ErrorMsg Expr
eval expr = do
  case expr of
    ExprLit (LitNum val) -> Right $ ExprLit (LitNum (val))
    ExprLit (LitBool val) -> Right $ ExprLit (LitBool (val))
    ExprLit (LitStr val) -> Right $ ExprLit (LitStr (val))
    ExprLit (LitNil) -> Right $ ExprLit (LitNil)
    ExprUn op x -> evalUnary op x
    ExprBin op x y -> evalBinary op x y
    ExprGroup x -> eval x

evalUnary :: UnOp -> Expr -> Either ErrorMsg Expr
evalUnary UnNeg x = do
  result <- eval x
  case result of
    ExprLit (LitNum val) -> Right $ ExprLit (LitNum (-val))
    _ -> Left $ mkErrorMsg (stringify result) "number"
evalUnary UnNot x = do
  -- if the expression evaluates to true we should return a false expression and vice versa
  result <- eval x
  case result of
    ExprLit (LitBool True) -> Right $ ExprLit (LitBool False)
    ExprLit (LitBool False) -> Right $ ExprLit (LitBool True)

evalBinary :: BinOp -> Expr -> Expr -> Either ErrorMsg Expr
evalBinary op x y = do
  -- Evaluate the operands
  x' <- eval x
  y' <- eval y

  case op of
    BinEq -> do
      checkComparable x' y'
      Right $ ExprLit (LitBool $ x' == y')
    BinNeq -> do
      checkComparable x' y'
      Right $ ExprLit (LitBool $ x' /= y')
    BinLt -> do
      xNum <- getNum x'
      yNum <- getNum y'
      Right $ ExprLit (LitBool $ xNum < yNum)
    BinLte -> do
      xNum <- getNum x'
      yNum <- getNum y'
      Right $ ExprLit (LitBool $ xNum <= yNum)
    BinGt -> do
      xNum <- getNum x'
      yNum <- getNum y'
      Right $ ExprLit (LitBool $ xNum > yNum)
    BinGte -> do
      xNum <- getNum x'
      yNum <- getNum y'
      Right $ ExprLit (LitBool $ xNum >= yNum)
    BinAdd -> do
      xNum <- getNum x'
      yNum <- getNum y'
      Right $ ExprLit (LitNum $ xNum + yNum)
    BinSub -> do
      xNum <- getNum x'
      yNum <- getNum y'
      Right $ ExprLit (LitNum $ xNum - yNum)
    BinMul -> do
      xNum <- getNum x'
      yNum <- getNum y'
      Right $ ExprLit (LitNum $ xNum * yNum)
    BinDiv -> do
      xNum <- getNum x'
      yNum <- getNum y'
      Right $ ExprLit (LitNum $ xNum / yNum)
    BinLogAnd -> do
      let xBool = isTruthy x'
          yBool = isTruthy y'
      Right $ ExprLit (LitBool $ xBool && yBool)
    BinLogOr -> do
      let xBool = isTruthy x'
          yBool = isTruthy y'
      Right $ ExprLit (LitBool $ xBool || yBool)

-- | This function determines if two Lox expressions
-- are comparable for the BinEq and BinNeq operations.
checkComparable :: Expr -> Expr -> Either ErrorMsg ()
checkComparable x y =
  case (x, y) of
    (ExprLit (LitNum _), ExprLit (LitNum _)) -> Right ()
    (ExprLit (LitStr _), ExprLit (LitStr _)) -> Right ()
    (ExprLit (LitBool _), ExprLit (LitBool _)) -> Right ()
    (ExprLit LitNil, _) -> Right ()
    (_, ExprLit LitNil) -> Right ()
    _ ->
      Left . ErrorMsg $
        "Operands: " <> stringify x <> " and " <> stringify y <> "are not comparable."

-- | When a number is expected, either provide 
-- an error message is a number is not found (Left),
-- otherwise return the number (Right).
getNum :: Expr -> Either ErrorMsg Float
getNum (ExprLit (LitNum x)) = Right x
getNum expr =
  Left . ErrorMsg $
    "Number expected but found: " <> stringify expr <> "."

-- | Cast a non-boolean Lox expression to a boolean for use
-- in boolean operations. 
-- The `nil` value is `false`, all other expressions are
-- considered `true`. Boolean values are either true or false
-- depending on their value.
isTruthy :: Expr -> Bool
isTruthy expr = 
  case expr of
    ExprLit LitNil -> False
    ExprLit (LitBool x) -> x
    ExprLit (LitStr _) -> True
    ExprLit (LitNum _) -> True
    ExprUn _ _ -> True
    ExprBin _ _ _ -> True
    ExprGroup _ -> True