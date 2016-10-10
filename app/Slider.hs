module Main where

import           Slidecoding             (load, ValidationMessage, Presentation(..), Metadata(..))

import           Control.Monad           ((>=>))
import qualified Data.Text.IO       as T
import           System.Environment      (getArgs)

main :: IO ()
main = getConfig >>= maybe usage checkPresentation
  where checkPresentation = load >=> printResult

printResult :: Either ValidationMessage Presentation -> IO ()
printResult = either putStrLn printPresentation

printPresentation :: Presentation -> IO ()
printPresentation p = do
  putStr "Generating presentation '"
  T.putStr . title . meta $ p
  putStrLn $ "' from " ++ rootDir p

usage :: IO ()
usage = putStrLn "Usage: slider directory"

getConfig :: IO (Maybe FilePath)
getConfig = parseArgs <$> getArgs

parseArgs :: [String] -> Maybe FilePath
parseArgs ("" : _) = Nothing
parseArgs (p : _) = Just p
parseArgs _ = Nothing
