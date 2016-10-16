{-# LANGUAGE FlexibleContexts #-}

module Slidecoding.Browser
    ( browseSignatures
    , browseSymbols
    , source
    ) where

import Slidecoding.Types

import Prelude hiding (until)

import qualified Language.Haskell.GhcMod       as GM  (GhcModT, browse)
import qualified Language.Haskell.GhcMod.Monad as GMM (runGmOutT, runGhcModT', withGhcModEnv)
import           Language.Haskell.GhcMod.Logging      (gmSetLogLevel, gmAppendLogQuiet)
import           Language.Haskell.GhcMod.Types        (GhcModLog, GhcModError, IOish, Options(..), OutputOpts(..), BrowseOpts(..), defaultOptions, defaultBrowseOpts, defaultGhcModState)

import Control.Arrow (first)
import Control.Monad.IO.Class (liftIO)
import Data.List              (isPrefixOf)
import Data.Char              (isSpace)

import System.Directory (canonicalizePath)
import System.FilePath  ((</>), (<.>))

browseSignatures :: Module -> IO [Signature]
browseSignatures = browse' Signature True

browseSymbols :: Module -> IO [Symbol]
browseSymbols = browse' Symbol False

browse' :: (String -> a) -> Bool -> Module -> IO [a]
browse' new detailed (Module wd m) = map new . lines <$> runGhcMod wd cmd
  where cmd = GM.browse opts m
        opts = defaultBrowseOpts { optBrowseDetailed = detailed }

source :: Module -> Symbol -> IO [String]
source m s = scopeTo s <$> readModule m

scopeTo :: Symbol -> [String] -> [String]
scopeTo (Symbol symbol) = until (anyOf [empty, otherPrefix]) . from (isPrefixOf prefix)
  where prefix = symbol ++ " "
        otherPrefix = allOf [not . isSpace . head, not . isPrefixOf prefix]
        from  criteria = dropWhile (not.criteria)
        until criteria = takeWhile (not.criteria)
        anyOf ps x = any ($x) ps
        allOf ps x = all ($x) ps

empty :: String -> Bool
empty = not . any (not.isSpace)

readModule :: Module -> IO [String]
readModule = load . modulePath

load :: FilePath -> IO [String]
load p = lines <$> readFile p

modulePath :: Module -> FilePath
modulePath (Module wd m) = wd </> "src" </> map dotToPath m <.> "hs"
  where dotToPath '.' = '/'
        dotToPath c = c

runGhcMod :: FilePath -> GM.GhcModT IO String -> IO String
runGhcMod wd = fmap extract . runGhcModT wd defaultOptions
  where extract = either (const "") id . fst

runGhcModT :: IOish m => FilePath -> Options -> GM.GhcModT m a -> m (Either GhcModError a, GhcModLog)
runGhcModT wd opt action = liftIO (canonicalizePath wd) >>= \dir' ->
  GMM.runGmOutT opt $
    GMM.withGhcModEnv dir' opt $ \(env,lg) ->
      first (fst <$>) <$> GMM.runGhcModT' env defaultGhcModState (do
        gmSetLogLevel (ooptLogLevel $ optOutput opt)
        gmAppendLogQuiet lg
        action)
