module Bli.Interpreter where

import Bli.Ast (
  Asgn (..),
  BinOp (..),
  Expr (..),
  LVal (LValVar),
  Literal (..),
  Stmt (..),
  UnOp (..),
  Var (..),
  stringify, UnExpr (UnExpr), BinExpr (BinExpr),
 )

import Prelude hiding (putStr, putStrLn)
--OLD
--import Control.Monad.Except (ExceptT, runExceptT, throwError)
--import Control.Monad.State (MonadIO (..), StateT (runStateT), gets, modify)
import Control.Monad.Except (ExceptT, runExceptT, throwError, MonadError)
import Control.Monad.State (MonadIO (..), StateT (runStateT), gets, modify, MonadState)
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HMap
import Data.Text.IO (putStr)
import Control.Monad (when, void)
import Bli.Error (ErrorMsg (ErrorMsg), mkErrorMsg)
import Data.Maybe (mapMaybe)
import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (stdout, hFlush)
import GHC.TypeLits (ErrorMessage(Text))
import qualified GHC.IO.Buffer as Hashmap
import GHC.Conc (numSparks)


type Mapping = HashMap Var Expr

newtype GlobalEnv = GlobalEnv {gMapping :: Mapping}
  deriving (Eq, Show)

newtype LocalEnv = LocalEnv {lMapping :: Mapping}
  deriving (Eq, Show)

-- | Recursively check for the `Var` value in the local
-- environments. Once the nested local environments are exhausted,
-- check the global environment.
lookupVar :: GlobalEnv -> [LocalEnv] -> Var -> Maybe Expr
lookupVar gEnv lEnvs var =
  lookupLocalVar lEnvs var <|> HMap.lookup var (gMapping gEnv)

lookupLocalVar :: [LocalEnv] -> Var -> Maybe Expr
lookupLocalVar envs var =
  case mapMaybe (HMap.lookup var . lMapping) envs of
    [] -> Nothing
    x : _ -> Just x

-- | There is aloways a base global environment
-- and there is always an active environment used for
-- assignments.
--
-- Any time a new scope is entered a new local
-- environment is created and will reference the previously
-- active environment as its parent environment.
-- The `environment` field should always reference the most-nested
-- environment. Whenever entering or exiting a scoped block, the
-- `environment` will be modified.
-- 
-- We could use a single `Environment` type to capture both 
-- the global and local environments, but then we could
-- potentially represent unintended nestings of environments.
data InterpreterState = InterpreterState
  { globalEnv :: GlobalEnv
  , localEnvs :: [LocalEnv]
  , errors :: [ErrorMsg]
  , debug :: Bool
  , collectOutput :: Bool
  , output :: [Text]
  }
  deriving (Eq, Show)

-- record syntax allows you to access the fields within (see the errors example)
initialInterpreterState :: InterpreterState
initialInterpreterState =
  InterpreterState
    { globalEnv = GlobalEnv HMap.empty
    , localEnvs = []
    , errors = []
    , debug = True
    , collectOutput = True
    , output = []
    }

--OLD
--type Interpreter = StateT InterpreterState IO
--type Interpreter = ExceptT ErrorMsg (StateT InterpreterState IO)
newtype Interpreter a = Interpreter
  { unInterpreter :: ExceptT ErrorMsg (StateT InterpreterState IO) a
  }
  deriving (Functor)
  deriving newtype
    ( Applicative
    , Monad
    , MonadFail
    , MonadError ErrorMsg
    , MonadIO
    , MonadState InterpreterState
    )

-- OLD 
-- runInterpreter :: ExceptT e (StateT s IO) a -> s -> IO (Either e a, s)
-- runInterpreter = runStateT . runExceptT

runInterpreter :: Interpreter a -> InterpreterState -> IO (Either ErrorMsg a, InterpreterState)
runInterpreter = runStateT . runExceptT . unInterpreter

interpret :: [Stmt Expr] -> IO ()
interpret = void . interpret' initialInterpreterState

interpret' :: InterpreterState -> [Stmt Expr] -> IO InterpreterState
interpret' startState stmts = do
  (result, finalState) <-
    runInterpreter
      (traverse_ execute stmts)
      startState
  case result of
    Left (ErrorMsg err) -> error $ T.unpack err
    Right () -> return finalState

interpretExpr :: Expr -> IO (Either ErrorMsg Expr)
interpretExpr expr = do
  (result, _finalState) <-
    runInterpreter (eval expr)
    initialInterpreterState
  return result

-- | Execute a single statement in the interpreter.
-- each statement is executed by this method
execute :: Stmt Expr -> Interpreter ()
execute stmt = do
  -- When the debug flag is on, statements are printed
  -- to the console.
  debugOn <- gets debug
  when debugOn (liftIO $ print stmt)
  case stmt of
    StmtPrint expr -> do
      -- Evaluate and print out the expression.
      result <- eval expr
      writeOutLn $ stringify result
    StmtDecl var expr -> do
      -- Evaluate the expression and define the variable.
      result <- eval expr
      defVar var result
    StmtExpr expr -> do
      -- Evaluate the expression, presumably for side effects
      -- such as variable assignment. The final evaluation result
      -- is not used.
      _ <- eval expr
      return ()
    StmtBlock stmts -> do
      -- StmtBlock [Stmt a]
      -- Push on a new environment block to the environment stack, 
      pushEnv
      -- execute the statements in the block, 
      traverse_ execute stmts
      -- and finally pop the environment block.
      popEnv

writeOut :: Text -> Interpreter ()
writeOut str = do
  collectOutputOn <- gets collectOutput
  -- If debug is enabled, log the output to the state
  if collectOutputOn
    then
      modify
          (\s -> s{output = str : output s})
    else do 
      liftIO $ putStr str
      liftIO $ hFlush stdout

-- | Use this function to write output from within
-- the interpreter.
-- This is a replacement for `putStrLn`.
writeOutLn :: Text -> Interpreter ()
writeOutLn str = writeOut $ str <> "\n"

pushEnv :: Interpreter ()
pushEnv = do 
  lEnvs <- gets localEnvs
  --let numLocalEnvs = fromIntegral (length lEnvs)
  --writeOutLn $ stringify (ExprLit (LitStr "calling push env"))
  --writeOutLn $ stringify (ExprLit (LitStr "num local envs"))
  --writeOutLn $ stringify (ExprLit (LitNum numLocalEnvs))
  --writeOutLn $ stringify (ExprLit (LitStr "calling push env"))
  modify (\s -> s{localEnvs = LocalEnv HMap.empty : localEnvs s})

popEnv :: Interpreter ()
popEnv = do
    lEnvs <- gets localEnvs
    gEnvs <- gets globalEnv 
    case lEnvs of
      [] -> return ()
      -- _x :: LocalEnv, xs :: [LocalEnv]
      (_x:xs) -> do
        --writeOutLn $ stringify (ExprLit (LitStr "popping an environment"))
        let numLocalEnvs = fromIntegral (length lEnvs)
        --writeOutLn $ stringify (ExprLit (LitNum numLocalEnvs))
        if numLocalEnvs == 1
          then 
            modify (\s -> s{localEnvs = []})
          else do 
            modify (\s -> s{localEnvs = xs})

addError :: ErrorMsg -> Interpreter ()
addError errorMsg =
  -- s is interpreter state
  -- leave s as it is, except update the fields inside the curly braces
  -- in this case modify the errors field, take the new errorMsg 
  --and put it at the front of the errors that we've collected thusfar
  -- \s ->
  -- this is an anonymous function definition, lambda syntax
  -- best practice is to only use anonymous functions for short operations
  modify (\s -> s{errors = errorMsg : errors s})

-- define a new variable with its initial value in the environment
defVar :: Var -> Expr -> Interpreter ()
defVar var expr = do
  lEnvs <- gets localEnvs
  -- lEnvs :: [LocalEnv]
  case lEnvs of 
  -- in the case that localEnvs is empty [], set the var in the global env 
    [] -> do 
      -- DEBUGGING
      --writeOutLn $ stringify (ExprLit (LitStr "adding a var to the global environment"))
      --writeOutLn $ stringify expr
      gEnvs <- gets globalEnv 
      modify ( \s -> s{globalEnv = gEnvs{gMapping = HMap.insert var expr (gMapping gEnvs)}})
  -- if local env, get the head and add a new var to the hmap
    x : xs -> do
      -- DEBUGGING
      --writeOutLn $ stringify (ExprLit (LitStr "adding a variable to the local environment"))
      --writeOutLn $ stringify expr
      modify ( \s -> s{localEnvs = x{lMapping = HMap.insert var expr (lMapping x)} : xs})

-- | Assign a new value to an existing variable. If the variable does not exist,
-- then the assignment shall _not_ define a new variable with the name.
assignVar :: Var -> Expr -> Interpreter ()
assignVar var@(Var vName) val = do
  gEnv <- gets globalEnv
  lEnvs <- gets localEnvs
  case assignLocalVar lEnvs var val of
    Just lEnvs' -> do
      modify ( \s -> s{localEnvs = lEnvs'})
    Nothing -> case assignGlobalVar gEnv var val of
      Just gEnv' -> do
        modify ( \s -> s{globalEnv = gEnv'})
      Nothing -> 
        throwError . ErrorMsg $ 
          "No variable (" <> vName <> ") found for assignment."

assignLocalVar :: [LocalEnv] -> Var -> Expr -> Maybe [LocalEnv]
assignLocalVar envs var val =
  case remEnvs of
    x : xs -> Just $ preEnvs ++ [x{lMapping = HMap.insert var val (lMapping x)}] ++ xs
    _ -> Nothing
    where
      (preEnvs, remEnvs) = break (HMap.member var . lMapping) envs

assignGlobalVar :: GlobalEnv -> Var -> Expr -> Maybe GlobalEnv
assignGlobalVar env var val =
  if HMap.member var (gMapping env) then
    Just $ env{gMapping = HMap.insert var val (gMapping env)}
  else
    Nothing

-- | Recursively evaluate an expression.
-- requires that the eval function neds to be run in an Interpreter environment 
eval :: Expr -> Interpreter Expr
eval expr = do
  case expr of
    ExprLit _lit -> return expr
    ExprUn (UnExpr op x) -> evalUnary op x
    ExprBin (BinExpr op x y) -> evalBinary op x y
    ExprGroup x -> eval x
    ExprVar var@(Var name) -> do
      gEnv <- gets globalEnv
      lEnvs <- gets localEnvs
      case lookupVar gEnv lEnvs var of
        Just val -> return val
        Nothing ->
          throwError
            ( ErrorMsg $
                "No variable named \'"
                  <> name
                  <> "\' found."
            )
    ExprAsgn (Asgn (LValVar var) right) -> do
      val <- eval right
      assignVar var val
      return val

evalUnary :: UnOp -> Expr -> Interpreter Expr
evalUnary UnNeg x = do
  result <- eval x
  case result of
    ExprLit (LitNum val) -> return $ ExprLit (LitNum (-val))
    _ -> throwError $ mkErrorMsg (stringify result) "number"
evalUnary UnNot x = do
  result <- eval x
  return $ ExprLit (LitBool $ not . isTruthy $ result)

evalBinary :: BinOp -> Expr -> Expr -> Interpreter Expr
evalBinary op x y = do
  x' <- eval x
  y' <- eval y

  case op of
    BinEq -> do
      checkEq x' y'
      return $ ExprLit (LitBool $ x' == y')
    BinNeq -> do
      checkEq x' y'
      return $ ExprLit (LitBool $ x' /= y')
    BinLt -> do
      xNum <- getNum x'
      yNum <- getNum y'
      return $ ExprLit (LitBool $ xNum < yNum)
    BinLte -> do
      xNum <- getNum x'
      yNum <- getNum y'
      return $ ExprLit (LitBool $ xNum <= yNum)
    BinGt -> do
      xNum <- getNum x'
      yNum <- getNum y'
      return $ ExprLit (LitBool $ xNum > yNum)
    BinGte -> do
      xNum <- getNum x'
      yNum <- getNum y'
      return $ ExprLit (LitBool $ xNum >= yNum)
    BinAdd -> do
      case x' of
        ExprLit (LitNum _)-> do
          xNum <- getNum x'
          yNum <- getNum y'
          return $ ExprLit (LitNum $ xNum + yNum)
        ExprLit (LitStr _) -> do  
          xStr <- getStr x'
          yStr <- getStr y'
          return $ ExprLit (LitStr $ xStr <> yStr)
        _ -> throwError $ mkErrorMsg (stringify x') "number or string"
    BinSub -> do
      xNum <- getNum x'
      yNum <- getNum y'
      return $ ExprLit (LitNum $ xNum - yNum)
    BinMul -> do
      xNum <- getNum x'
      yNum <- getNum y'
      return $ ExprLit (LitNum $ xNum * yNum)
    BinDiv -> do
      xNum <- getNum x'
      yNum <- getNum y'
      return $ ExprLit (LitNum $ xNum / yNum)
    BinLogAnd -> do
      xBool <- getBool x'
      yBool <- getBool y'
      return $ ExprLit (LitBool $ xBool && yBool)
    BinLogOr -> do
      xBool <- getBool x'
      yBool <- getBool y'
      return $ ExprLit (LitBool $ xBool || yBool)

-- | This function determines if two Lox expressions
-- are comparable for the BinEq and BinNeq operations.
checkEq :: Expr -> Expr -> Interpreter ()
checkEq x y =
  case (x, y) of
    (ExprLit (LitNum _), ExprLit (LitNum _)) -> return ()
    (ExprLit (LitStr _), ExprLit (LitStr _)) -> return ()
    (ExprLit (LitBool _), ExprLit (LitBool _)) -> return ()
    (ExprLit LitNil, _) -> return ()
    (_, ExprLit LitNil) -> return ()
    _ ->
      throwError . ErrorMsg $
        "Operands: " <> stringify x <> " and " <> stringify y <> "are not comparable."

-- | When a number is expected, either throw 
-- an error message when a number is not found
-- or return the number.
getNum :: Expr -> Interpreter Float
getNum (ExprLit (LitNum x)) = return x
getNum expr =
  throwError . ErrorMsg $
    "Number expected but found: " <> stringify expr <> "."

-- | When a string is expected, either throw 
-- an error message that a string is not found
-- or return the string.
getStr :: Expr -> Interpreter Text
getStr (ExprLit (LitStr s)) = return s
getStr expr =
  throwError . ErrorMsg $
    "String expected but found: " <> stringify expr <> "."

getBool :: Expr -> Interpreter Bool
getBool (ExprLit (LitBool x)) = return x
getBool expr = return $ isTruthy expr

-- | Cast a non-boolean Lox expression to a boolean for use
-- in boolean operations. 
-- The `nil` value is `false`, all other expressions are
-- considered `true`. Boolean values are either true or false
-- depending on their value.
isTruthy :: Expr -> Bool
isTruthy (ExprLit LitNil) = False
isTruthy (ExprLit (LitBool x)) = x
isTruthy _ = True