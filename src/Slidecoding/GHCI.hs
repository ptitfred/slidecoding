module Slidecoding.GHCI
    ( Stream(..)
    , run
    , ioStream
    ) where

import Prelude hiding (fail, interact)

import Slidecoding.ReplSession
import Slidecoding.Types

import Control.Monad          (forever, void, when)
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
  | otherwise     = liftIO (evalInSession s msg)

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
handleCommand ("/load": ctxName : _) c s = do
  sayIO ("/load " ++ ctxName)
  loadCtx ctxName c s
handleCommand (cmd:_) _ _ = fail ("Unknown command " ++ cmd)
handleCommand []      _ _ = fail "No command"

help :: Monad m => m (Either String String)
help = say $ unlines [ "/load contextName : load modules from the context name"
                     , "/help, /? : this message"
                     ]

sayIO :: MonadIO m => String -> m ()
sayIO = liftIO.putStrLn

say, fail :: Monad m => String -> m (Either String String)
say = return . Right
fail = return . Left

loadCtx :: (Monad m, MonadIO m) => ModuleName -> Context -> ReplSession -> m (Either String String)
loadCtx moduleName ctx session = importModules session moduleName ctx

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
importModules :: (Monad m, MonadIO m) => ReplSession -> ModuleName -> Context -> m (Either String String)
importModules s mn ctx = liftIO $ do
  evalSilent  ":load" -- Resets the context
  evalSilent (":load " ++ unwords ms)
  evalAllWith importModule ms
  where ms = mn : tms
        tms = topLevelModules ctx
        importModule m = "import " ++ m
        eval = evalInSession s
        evalSilent = void . eval
        evalAllWith f = reduceMWith (eval.f)

reduceMWith :: (Monad m) => (a -> m (Either e String)) -> [a] -> m (Either e String)
reduceMWith f = reduceM . map f

reduceM :: (Monad m) => [m (Either e String)] -> m (Either e String)
reduceM actions = reduceM' actions (Right "")

reduceM' :: (Monad m) => [m (Either e v)] -> Either e v -> m (Either e v)
reduceM' (a:as) (Right _) = a >>= reduceM' as
reduceM' _      r         = return r
