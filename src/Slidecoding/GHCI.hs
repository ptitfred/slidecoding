module Slidecoding.GHCI
    ( Stream(..)
    , run
    , ioStream
    ) where

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
  loadCtx ctx session >>= either printError nothing
  forever $ evalInput session
  where evalInput s = readInput stream >>= liftIO . flip evalInSession s >>= printResult
        printResult = either (writeOutput stream) (writeOutput stream)
        printError e = liftIO $ putStr "Error: " >> print e

nothing :: Monad m => a -> m ()
nothing _ = return ()

loadCtx :: (Monad m, MonadIO m) => Context -> ReplSession -> m (Either String String)
loadCtx ctx session = importModules session (modules ctx)

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
