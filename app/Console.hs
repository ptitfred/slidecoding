module Main (main) where

import Slidecoding        (start)
import Slidecoding.Types

import Data.Char          (isDigit)
import System.Environment (getArgs)

main :: IO ()
main = maybe help run =<< getConfig

help :: IO ()
help = do
  putStrLn "Usage:"
  putStrLn ""
  putStrLn "  console port ModuleName"

type Config = (Port, ModuleName)

run :: Config -> IO ()
run (port, moduleName) = do
  let url = "Listening on ws://localhost:" ++ show port ++ "/"
  let context = singleModuleContext moduleName
  putStrLn url
  start port context
  putStrLn "Bye"

getConfig :: IO (Maybe Config)
getConfig = parseArgs <$> getArgs

parseArgs :: [String] -> Maybe Config
parseArgs ("" : "" : _)                 = Nothing
parseArgs ( p : mn : _) | all isDigit p = Just (read p, mn)
parseArgs _                             = Nothing
