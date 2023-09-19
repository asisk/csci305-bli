module Main where

import Bli.Ast (eval, stringify)
import Bli.Parse (pExpr)

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (parse)
import System.IO (hFlush, stdout)

prompt :: Text -> IO Text
prompt t = do
  putStr $ T.unpack t
  hFlush stdout
  T.pack <$> getLine

repl :: IO ()
repl = do
  input <- prompt "> "
  case input of 
    ":q" -> return ()
    _ -> do
      case parse pExpr "" input of
        Right expr -> do
          let result = eval expr
          case result of 
            Right val -> putStrLn . T.unpack $ stringify val
            Left err -> print err
          repl
        Left err -> do
          print err

main :: IO ()
main = repl