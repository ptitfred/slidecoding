module Main (main) where

import Slidecoding        (browse, indexIO, loadExposedModules, load, processSlides, start)
import Slidecoding.Types

import Control.Concurrent (forkIO)
import Control.Monad      ((>=>), void)
import Data.List          (isSuffixOf)
import System.Directory   (createDirectoryIfMissing, getDirectoryContents, doesDirectoryExist)
import System.Environment (getArgs)
import System.FilePath    ((</>))
import qualified Network.Wai.Handler.Warp as Warp (run)
import qualified Network.Wai.Application.Static as Wai (staticApp, defaultFileServerSettings)

data Config = Config FilePath Action
type Action = FilePath -> IO ()

main :: IO ()
main = withConfig run

run :: Config -> IO ()
run (Config file action) = action file

check :: Action
check f = do
  checkPresentation f
  withProject f $ \(_, slides, descs) -> do
    newline
    putStrLn "Slides:"
    describeSlides slides
    newline
    putStrLn "Modules:"
    describeModules descs

newline :: IO ()
newline = putStrLn ""

process :: Action
process dir = withProject dir (process' Nothing)

process' :: Maybe Port -> Project -> IO ()
process' port (presentation, slides, descs) = do
  createDirectoryIfMissing True (distDir presentation)
  processSlides port presentation descs slides

index :: Action
index path = loadExposedModules path >>= maybe noModule someModules
  where noModule = putStrLn "No modules found"
        someModules = mapM_ indexModule
        indexModule m = indexIO m (path </> "tmp")

serve :: Action
serve f = withProject f $ \ project@(presentation, _, descs) -> do
  let httpPort = 3000
  let wsPort = httpPort + 1
  process' (Just wsPort) project
  putStrLn ("Serving " ++ f ++ " on http://localhost:" ++ show httpPort ++ "/")
  serveWS wsPort (buildContext f descs)
  serveStatic httpPort (distDir presentation)

serveWS :: Port -> Maybe Context -> IO ()
serveWS _     Nothing       = return ()
serveWS port (Just context) = forkIO_ (start port context)

forkIO_ :: IO () -> IO ()
forkIO_ = void . forkIO

buildContext :: FilePath -> [Description] -> Maybe Context
-- TODO multi modules context
buildContext f (Description (Module _ m) _ : _) = Just (singleModuleContext f m)
buildContext _ _                                = Nothing

serveStatic :: Port -> FilePath -> IO ()
serveStatic port directory = Warp.run port waiApp
  where waiApp = Wai.staticApp (Wai.defaultFileServerSettings directory)

type Project = (Presentation, [FilePath], [Description])

withProject :: FilePath -> (Project -> IO ()) -> IO ()
withProject f action = onlyWithDirectory f $ loadProject f >>= either putStrLn action

type Check = Either ValidationMessage

loadProject :: FilePath -> IO (Check Project)
loadProject f = load f >>= loadProject' f

loadProject' :: FilePath -> Check Presentation -> IO (Check Project)
loadProject' f (Right p) = right <$> loadSlides f <*> loadModules f
  where right s m = Right (p, s, m)
loadProject' _ (Left vm) = return (Left vm)

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

printResult :: Check Presentation -> IO ()
printResult = either putStrLn printPresentation

printPresentation :: Presentation -> IO ()
printPresentation p = do
  putStr "Generating presentation '"
  putStr . title . metadata $ p
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
parseAction "check"   = Just check
parseAction "process" = Just process
parseAction "index"   = Just index
parseAction "serve"   = Just serve
parseAction _         = Nothing
