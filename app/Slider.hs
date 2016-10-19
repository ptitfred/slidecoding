module Main (main) where

import           Slidecoding             (Description(..), Module(..), index, loadExposedModules, load, ValidationMessage, Presentation(..), Metadata(..))
import           Slidecoding.SlidesWriter

import           Control.Monad           ((>=>), when)
import           Data.List               (isSuffixOf)
import qualified Data.Text.IO       as T
import           System.Environment      (getArgs)
import           System.FilePath         ((</>))
import           System.Directory        (getDirectoryContents, doesDirectoryExist)

import           Text.Pandoc

data Config = Config FilePath Action
data Action = Check | ProcessSlides

main :: IO ()
main = withConfig run

run :: Config -> IO ()
run (Config f Check)         = check f
run (Config f ProcessSlides) = processSlides f

check :: FilePath -> IO ()
check f = do
  checkPresentation f
  withProject f $ \(slides, descs) -> do
    newline
    putStrLn "Slides:"
    describeSlides slides
    newline
    putStrLn "Modules:"
    describeModules descs

newline :: IO ()
newline = putStrLn ""

processSlides :: FilePath -> IO ()
processSlides f = withProject f $ \(slides, descs) -> mapM_ (processSlide descs) slides

type Project = ([FilePath], [Description])

withProject :: FilePath -> (Project -> IO ()) -> IO ()
withProject f action = onlyWithDirectory f $ loadProject f >>= action

loadProject :: FilePath -> IO Project
loadProject f = (,) <$> loadSlides f <*> loadModules f

onlyWithDirectory :: FilePath -> IO () -> IO ()
onlyWithDirectory f action = do
  good <- doesDirectoryExist f
  when good action

processSlide :: [Description] -> FilePath -> IO ()
processSlide descs file = readFile file >>= putStrLn . filterSlide descs

filterSlide :: [Description] -> String -> String
filterSlide descs = writeDoc . walkSlides descs . readDoc

readDoc :: String -> Pandoc
readDoc s = case readMarkdown def s of
                 Right doc -> doc
                 Left err  -> error (show err)

writeDoc :: Pandoc -> String
writeDoc = writeMarkdown def

checkPresentation :: FilePath -> IO ()
checkPresentation = load >=> printResult

loadModules :: FilePath -> IO [Description]
loadModules path = do
  modules <- loadExposedModules path
  maybe (return []) (mapM index) modules

describeModules :: [Description] -> IO ()
describeModules []      = putStrLn "  No modules"
describeModules modules = printDescriptions modules

loadSlides :: FilePath -> IO [FilePath]
loadSlides path = do
  files <- listFiles (suffixedBy "md") $ path </> "slides"
  return . maybe [] (map (\f -> path </> "slides" </> f)) $ files

describeSlides :: [FilePath] -> IO ()
describeSlides []     = putStrLn "  No slides"
describeSlides slides = printFiles slides

suffixedBy :: String -> String -> Bool
suffixedBy = isSuffixOf

listFiles :: (String -> Bool) -> FilePath -> IO (Maybe [FilePath])
listFiles predicate path = do
  goOn <- doesDirectoryExist path
  if goOn
  then Just . filter predicate <$> getDirectoryContents path
  else return Nothing

withConfig :: (Config -> IO ()) -> IO ()
withConfig action = getConfig >>= maybe usage action

printResult :: Either ValidationMessage Presentation -> IO ()
printResult = either putStrLn printPresentation

printPresentation :: Presentation -> IO ()
printPresentation p = do
  putStr "Generating presentation '"
  T.putStr . title . meta $ p
  putStrLn $ "' from " ++ rootDir p

printDescriptions :: [Description] -> IO ()
printDescriptions [] = return ()
printDescriptions (Description (Module _ m) _ : ds) = do
  putStr "  "
  putStrLn m
  printDescriptions ds

printFiles :: [FilePath] -> IO ()
printFiles [] = return ()
printFiles (f:fs) = do
  putStr "  "
  putStrLn f
  printFiles fs

usage :: IO ()
usage = putStrLn "Usage: slider <check|process> directory"

getConfig :: IO (Maybe Config)
getConfig = parseArgs <$> getArgs

parseArgs :: [String] -> Maybe Config
parseArgs ("" : "" : _) = Nothing
parseArgs (a  : p  : _) = Config p <$> parseAction a
parseArgs _             = Nothing

parseAction :: String -> Maybe Action
parseAction "check"   = Just Check
parseAction "process" = Just ProcessSlides
parseAction _         = Nothing
