module Main (main) where

import           Slidecoding             (browse, indexIO, loadExposedModules, load, processSlides, ValidationMessage, Presentation(..), Metadata(..))
import           Slidecoding.Types

import           Control.Monad           ((>=>))
import           Data.List               (isSuffixOf)
import qualified Data.Text.IO       as T
import           System.Environment      (getArgs)
import           System.FilePath         ((</>))
import           System.Directory        (createDirectoryIfMissing, getDirectoryContents, doesDirectoryExist)

data Config = Config FilePath Action
data Action = Check | ProcessSlides | Index | Serve

main :: IO ()
main = withConfig run

run :: Config -> IO ()
run (Config f Check)         = check f
run (Config f ProcessSlides) = process f
run (Config f Index)         = index f
run (Config f Serve)         = serve f

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

process :: FilePath -> IO ()
process dir = withProject dir $ \(slides, descs) -> do
  createDirectoryIfMissing True distDir
  processSlides descs slides outFile
    where distDir = dir </> "dist"
          outFile = distDir </> "index.html"

index :: FilePath -> IO ()
index path = loadExposedModules path >>= maybe noModule someModules
  where noModule = putStrLn "No modules found"
        someModules = mapM_ indexModule
        indexModule m = indexIO m (path </> "tmp")

serve :: FilePath -> IO ()
serve f = withProject f $ \_ -> putStrLn ("Serving " ++ f)

type Project = ([FilePath], [Description])

withProject :: FilePath -> (Project -> IO ()) -> IO ()
withProject f action = onlyWithDirectory f $ loadProject f >>= action

loadProject :: FilePath -> IO Project
loadProject f = (,) <$> loadSlides f <*> loadModules f

onlyWithDirectory :: FilePath -> IO () -> IO ()
onlyWithDirectory f action = do
  good <- doesDirectoryExist f
  if good
  then action
  else putStrLn ("No slidecoding project in " ++ f)

checkPresentation :: FilePath -> IO ()
checkPresentation = load >=> printResult

loadModules :: FilePath -> IO [Description]
loadModules path = loadExposedModules path >>= maybe (return []) (mapM browse)

describeModules :: [Description] -> IO ()
describeModules []    = putStrLn "  No modules"
describeModules descs = printDescriptions descs

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
usage = putStrLn "Usage: slider <check|process|index|serve> directory"

getConfig :: IO (Maybe Config)
getConfig = parseArgs <$> getArgs

parseArgs :: [String] -> Maybe Config
parseArgs ("" : "" : _) = Nothing
parseArgs (a  : p  : _) = Config p <$> parseAction a
parseArgs _             = Nothing

parseAction :: String -> Maybe Action
parseAction "check"   = Just Check
parseAction "process" = Just ProcessSlides
parseAction "index"   = Just Index
parseAction "serve"   = Just Serve
parseAction _         = Nothing
