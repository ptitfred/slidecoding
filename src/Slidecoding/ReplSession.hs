module Slidecoding.ReplSession
    ( ReplSession(..)
    , evalInSession
    , startSession
    , endSession
    ) where

 {-
  - Original work from https://github.com/jetaggart/light-haskell/blob/master/haskell/ReplSession.hs
  -}

import System.IO
import System.Process
import System.Directory (getDirectoryContents)
import Data.List (isSuffixOf)
import Control.Monad (void)
import Language.Haskell.Exts

data ReplSession = ReplSession { replIn      :: Handle
                               , replOut     :: Handle
                               , replError   :: Handle
                               , replProcess :: ProcessHandle
                               }

evalInSession :: String -> ReplSession -> IO (Either String String)
evalInSession cmd session@(ReplSession _ out err _) = do
  sendCommand (":{\n" ++ prepareCode cmd ++ "\n") session
  clearHandle out 10
  clearHandle err 0
  sendCommand ":}\n" session
  readEvalOutput session

prepareCode :: String -> String
prepareCode code = case parseCode code of
  [] -> code
  decls -> if isFunDecl $ head decls
           then prependLet decls
           else code

isFunDecl :: Decl -> Bool
isFunDecl TypeSig {} = True
isFunDecl FunBind {} = True
isFunDecl PatBind {} = True
isFunDecl _ = False

prependLet :: [Decl] -> String
prependLet = prettyPrint . letStmt

parseCode :: String -> [Decl]
parseCode code = case parseFileContents code of
  (ParseOk (Module _ _ _ _ _ _ decls)) -> decls
  ParseFailed {} -> []

readEvalOutput :: ReplSession -> IO (Either String String)
readEvalOutput (ReplSession _ out err _) = do
  output <- readUntil out ("--EvalFinished\n" `isSuffixOf`)
  let onlyOutput = take (length output - length "--EvalFinished\n") output
  hasErrorOutput <- hReady err
  if hasErrorOutput
    then readAll err >>= \errorOutput -> return . Left $ errorOutput
    else return . Right $ onlyOutput

readUntil :: Handle -> (String -> Bool) -> IO String
readUntil handle = readUntil' handle ""

readUntil' :: Handle -> String -> (String -> Bool) -> IO String
readUntil' handle output predicate = do
  char <- hGetChar handle
  let newOutput = output ++ [char]
  if predicate newOutput
    then return newOutput
    else readUntil' handle newOutput predicate

readAll :: Handle -> IO String
readAll handle = untilM' (not <$> hReady handle) (hGetChar handle)

startSession :: FilePath -> IO ReplSession
startSession path = do
  (cmd, args) <- replCommand path
  (input, out, err, process) <- runInteractiveProcess cmd args (Just path) Nothing
  let session = ReplSession input out err process
  prepareSession session
  return session

replCommand :: FilePath -> IO (String, [String])
replCommand dir = choose <$> isStackProject dir <*> isCabalProject dir
  where choose True  _     = ("stack", ["repl"])
        choose False True  = ("cabal", ["repl"])
        choose False False = ("ghci", [])

isCabalProject :: FilePath -> IO Bool
isCabalProject dir = any (".cabal" `isSuffixOf`) <$> getDirectoryContents dir

isStackProject :: FilePath -> IO Bool
isStackProject dir = elem "stack.yaml" <$> getDirectoryContents dir

prepareSession :: ReplSession -> IO ()
prepareSession session@(ReplSession _ out _ _) = do
  sendCommand ":set prompt \"--EvalFinished\\n\"\n" session
  clearHandle out 1000

sendCommand :: String -> ReplSession -> IO ()
sendCommand cmd (ReplSession input _ _ _) = do
  hPutStrLn input cmd
  hFlush input

clearHandle :: Handle -> Int -> IO ()
clearHandle handle wait =
  untilM (not <$> hWaitForInput handle wait) $ hGetChar handle

untilM :: (Monad m) => m Bool -> m a -> m ()
untilM predicate action = void (untilM' predicate action)

untilM' :: (Monad m) => m Bool -> m a -> m [a]
untilM' predicate action = do
  isFinished <- predicate
  if isFinished
    then return []
    else do
      res <- action
      others <- untilM' predicate action
      return $ res : others

endSession :: ReplSession -> IO ()
endSession session = do
  sendCommand ":quit\n" session
  void $ waitForProcess $ replProcess session
