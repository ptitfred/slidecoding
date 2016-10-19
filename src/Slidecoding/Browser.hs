{-# LANGUAGE FlexibleContexts #-}

module Slidecoding.Browser
    ( browse
    ) where

import Slidecoding.Types

import Prelude hiding (until)

import qualified Language.Haskell.GhcMod       as GM  (GhcModT, browse)
import qualified Language.Haskell.GhcMod.Monad as GMM (runGmOutT, runGhcModT', withGhcModEnv)
import           Language.Haskell.GhcMod.Logging      (gmSetLogLevel, gmAppendLogQuiet)
import           Language.Haskell.GhcMod.Types        (GhcModLog, GhcModError, IOish, Options(..), OutputOpts(..), BrowseOpts(..), defaultOptions, defaultBrowseOpts, defaultGhcModState)

import Codec.Binary.Base64.String as B64 (encode)
import Control.Arrow          (first)
import Control.Monad.IO.Class (liftIO)
import Data.Char              (isSpace)
import Data.List              (intercalate, isPrefixOf)

import System.Directory (canonicalizePath)
import System.FilePath  ((</>), (<.>))

browse :: Module -> IO Description
browse m = withSymbol <$> browseSignatures m >>= loadDescription m

type Interface = (Symbol, Signature)

withSymbol :: [Signature] -> [Interface]
withSymbol = map decorate
  where decorate (Signature sig) = (symbol sig, Signature sig)
        symbol = Symbol . unwords . takeWhile (/= "::") . words

loadDescription :: Module -> [Interface] -> IO Description
loadDescription m interface = Description m <$> loadItems m interface

loadItems :: Module -> [Interface] -> IO [Item]
loadItems m interface = loadAll <$> readModule m
  where loadAll content = map (loadItem content) interface

loadItem :: [String] -> Interface -> Item
loadItem content (s, sig) = Item s sig . Source . base64 . intercalate "\n" $ scopeTo s content

base64 :: String -> String
base64 = filter (not . isSpace) . B64.encode

browseSignatures :: Module -> IO [Signature]
browseSignatures (Module wd m) = map Signature . lines <$> runGhcMod wd cmd
  where cmd = GM.browse opts m
        opts = defaultBrowseOpts { optBrowseDetailed = True }

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
