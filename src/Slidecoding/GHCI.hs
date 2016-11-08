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
import Data.List              (find)
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

run :: (Monad m, MonadIO m) => Stream m -> Presentation -> m ()
run stream presentation = do
  prepare stream
  session <- liftIO (startSession (rootDir presentation))
  forever $ evalInput stream ctx session
    where ctx = contexts (metadata presentation)

evalInput :: (Monad m, MonadIO m) => Stream m -> [Context] -> ReplSession -> m ()
evalInput stream ctxs session = getCommand >>= interact >>= writeResult
  where getCommand = readInput stream
        interact = handle ctxs session
        writeResult = printResult stream

printResult :: (Monad m, MonadIO m) => Stream m -> Either String String -> m ()
printResult stream = either (writeOutput stream) (writeOutput stream)

handle :: (Monad m, MonadIO m) => [Context] -> ReplSession -> String -> m (Either String String)
handle cs s msg
  | isCommand msg = handleCommand (words msg) cs s
  | otherwise     = liftIO (evalInSession s msg)

isCommand :: String -> Bool
isCommand ('/' : a : _) = a /= ' '
isCommand _ = False

{-
 Commands:
   /load contextName
   /help, /?
 -}
handleCommand :: (Monad m, MonadIO m) => [String] -> [Context] -> ReplSession -> m (Either String String)
handleCommand ("/help": _) _ _ = help
handleCommand ("/?": _)    _ _ = help
handleCommand ("/load": "prelude": _) _ s = do
  sayIO "/load prelude"
  loadContext s prelude
handleCommand ("/load": ctxName : _) cs s = do
  sayIO ("/load " ++ ctxName)
  maybe (fail notFound) (loadContext s) ctx
    where ctx = find (byName ctxName) cs
          byName n c = name c == n
          notFound = "Context " ++ ctxName ++ " undefined"
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
loadContext :: (Monad m, MonadIO m) => ReplSession -> Context -> m (Either String String)
loadContext s ctx = liftIO $ do
  evalSilent  ":load" -- Resets the context
  evalSilent (":load " ++ unwords sources)
  evalAllWith importModule allModules
  where allModules = sources ++ others
        sources = srcModules ctx
        others = otherModules ctx
        importModule m = "import " ++ m
        eval m = putStrLn m >> evalInSession s m
        evalSilent = void . eval
        evalAllWith f = reduceMWith (eval.f)

reduceMWith :: (Monad m) => (a -> m (Either e String)) -> [a] -> m (Either e String)
reduceMWith f = reduceM . map f

reduceM :: (Monad m) => [m (Either e String)] -> m (Either e String)
reduceM actions = reduceM' actions (Right "")

reduceM' :: (Monad m) => [m (Either e v)] -> Either e v -> m (Either e v)
reduceM' (a:as) (Right _) = a >>= reduceM' as
reduceM' _      r         = return r
