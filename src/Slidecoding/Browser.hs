module Slidecoding.Browser
    ( browseSignatures
    , browseSymbols
    , source
    ) where

import Slidecoding.Types

import qualified Language.Haskell.GhcMod as GM (GhcModT, browse, runGhcModT)
import Language.Haskell.GhcMod.Types (BrowseOpts(..), defaultOptions, defaultBrowseOpts)
import Data.List (isPrefixOf)
import Data.Char (isSpace)
import Prelude hiding (until)

browseSignatures :: Module -> IO [Signature]
browseSignatures = browse' Signature True

browseSymbols :: Module -> IO [Symbol]
browseSymbols = browse' Symbol False

browse' :: (String -> a) -> Bool -> Module -> IO [a]
browse' new detailed (Module m) = map new . lines <$> runGhcMod cmd
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
modulePath (Module m) = "src/" ++ map dotToPath m ++ ".hs"
  where dotToPath '.' = '/'
        dotToPath c = c

runGhcMod :: GM.GhcModT IO String -> IO String
runGhcMod = fmap extract . GM.runGhcModT defaultOptions
  where extract = either (const "") id . fst
