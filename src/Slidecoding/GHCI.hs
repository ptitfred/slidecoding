module Slidecoding.GHCI
    ( Stream(..)
    , run
    , ioStream
    ) where

import Prelude hiding (fail, interact)

import Slidecoding.ReplSession
import Slidecoding.Types

import Control.Monad          (forever, when, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO              (BufferMode(..), hSetBuffering, stdout, stderr)

ioStream :: Bool -> Stream IO
ioStream withPrompt = Stream noBuffering input output
  where noBuffering = do
          putStrLn "starting"
          hSetBuffering stdout NoBuffering
          hSetBuffering stderr NoBuffering
        input  = do
          when withPrompt $ putStr "> "
          putStr "> "
          getLine
        output = putStr

prepare :: Monad m => Stream m -> m ()
prepare (Stream p _ _) = p

readInput :: Monad m => Stream m -> m String
readInput (Stream _ input _) = input

writeOutput :: Monad m => Stream m -> String -> m ()
writeOutput (Stream _ _ output) = output

run :: (Monad m, MonadIO m) => Stream m -> Context -> m ()
run stream ctx = do
  prepare stream
  session <- liftIO (startSession (workingDir ctx))
  loadCtx "" ctx session >>= printResult stream
  forever $ evalInput stream ctx session

evalInput :: (Monad m, MonadIO m) => Stream m -> Context -> ReplSession -> m ()
evalInput stream ctx session = getCommand >>= interact >>= writeResult
  where getCommand = readInput stream
        interact = handle ctx session
        writeResult = printResult stream

printResult :: (Monad m, MonadIO m) => Stream m -> Either String String -> m ()
printResult stream = either (writeOutput stream) (writeOutput stream)

handle :: (Monad m, MonadIO m) => Context -> ReplSession -> String -> m (Either String String)
handle c s msg
  | isCommand msg = handleCommand (words msg) c s
  | otherwise     = liftIO (evalInSession msg s)

isCommand :: String -> Bool
isCommand ('/' : a : _) = a /= ' '
isCommand _ = False

{-
 Commands:
   /load contextName
   /help, /?
 -}
handleCommand :: (Monad m, MonadIO m) => [String] -> Context -> ReplSession -> m (Either String String)
handleCommand ("/help": _) _ _ = help
handleCommand ("/?": _)    _ _ = help
handleCommand ("/load": ctxName : _) c s = loadCtx ctxName c s -- TODO use ctxName
handleCommand (cmd:_) _ _ = fail ("Unknown command " ++ cmd)
handleCommand []      _ _ = fail "No command"

help :: Monad m => m (Either String String)
help = say $ unlines [ "/load contextName : load modules from the context name"
                     , "/help, /? : this message"
                     ]

say, fail :: Monad m => String -> m (Either String String)
say = return . Right
fail = return . Left

loadCtx :: (Monad m, MonadIO m) => String -> Context -> ReplSession -> m (Either String String)
loadCtx _ ctx session = importModules session (modules ctx)

{-
  Boot sequence:

  Reset context to get rid of stack/cabal auto loading
  > :load

  Load our modules (to avoid noise) with the main module
  > :load mainModule dependentModule1 dependentModule2

  Import dependencies to have access to exposed functions
  > import dependentModule1
  > import dependentModule2

 -}
importModules :: (Monad m, MonadIO m) => ReplSession -> [ModuleName] -> m (Either String String)
importModules s ms = liftIO $ do
  -- TODO: see how to chain IO (Either a b)
  void $ evalInSession ":load" s
  void $ evalInSession (":load " ++ unwords ms) s
  reduce <$> mapM importModule ms
  where importModule m = evalInSession (cmd m) s
        cmd m = "import " ++ m

reduce :: [Either String String] -> Either String String
reduce = foldr failFast (Right "")
  where failFast (Left e)  _ = Left e
        failFast (Right _) r = r
