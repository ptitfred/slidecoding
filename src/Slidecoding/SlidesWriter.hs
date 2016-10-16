module Slidecoding.SlidesWriter
    ( walkSlides
    ) where

import Slidecoding.Types
import Slidecoding.Indexer               (Description(..), Item(..), Source(..))

import Codec.Binary.Base64.String as B64 (decode)

import Data.List                         (find, isPrefixOf)

import Text.Pandoc

walkSlides :: [Description] -> Pandoc -> Pandoc
walkSlides descs (Pandoc m bs) = Pandoc m $ concatMap (replaceSourceBlock descs) bs

replaceSourceBlock :: [Description] -> Block -> [Block]
replaceSourceBlock descs original@(Para [Link _ contents (url, _)]) | isSourceUrl url =
  case inlineLink descs url of
    Just b  -> [Para contents, b]
    Nothing -> [original]
replaceSourceBlock _ b = [b]

isSourceUrl :: String -> Bool
isSourceUrl url = "source://" `isPrefixOf` url

inlineLink :: [Description] -> String -> Maybe Block
inlineLink descs url = CodeBlock nullAttr <$> source
  where source = loadSource descs url

loadSource :: [Description] -> String -> Maybe String
loadSource descs url = parseUrl url >>= lookupSource descs

parseUrl :: String -> Maybe (ModuleName, String)
parseUrl url =
  case tokens of
    [m, f] -> Just (m, f)
    _      -> Nothing
  where tokens = splitOn '/' suffix
        suffix = drop (length "source://") url

splitOn :: Char -> String -> [String]
splitOn c s =
  case dropWhile (== c) s of
    "" -> []
    s' -> w : splitOn c s''
      where (w, s'') = break (== c) s'

lookupSource :: [Description] -> (ModuleName, String) -> Maybe String
lookupSource descs (m, f) = do
  Description _ items   <- find (ofModule m) descs
  Item _ _ (Source b64) <- find (ofFunction f) items
  return (B64.decode b64)

ofModule :: ModuleName -> Description -> Bool
ofModule m (Description (Module _ mn) _) = m == mn

ofFunction :: String -> Item -> Bool
ofFunction f (Item (Symbol s) _ _) = f == s
