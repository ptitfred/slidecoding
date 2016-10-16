{-# LANGUAGE OverloadedStrings #-}

module Slidecoding.Indexer
    ( Description(..)
    , Source(..)
    , Item(..)
    , index
    , indexIO
    ) where

import Slidecoding.Browser
import Slidecoding.Types

import Codec.Binary.Base64.String as B64 (encode)

import Data.Aeson                 hiding (encode)
import Data.Aeson                 as A   (encode)
import Data.ByteString.Lazy.Char8 as B   (hPutStrLn)
import Data.Char                         (isSpace)
import Data.List                         (intercalate)
import Data.Text                  as L   (pack)

import System.FilePath                   ((</>), (<.>))
import System.IO                         (IOMode(..), withFile)

data Description = Description Module [Item]
data Item = Item Symbol Signature Source
newtype Source = Source String

index :: Module -> IO Description
index m = do
  ss   <- browseSymbols m
  sigs <- browseSignatures m
  loadDescription m ss sigs

indexIO :: Module -> FilePath -> IO ()
indexIO m dir = do
  description <- index m
  writeIndexJson dir description
  mapM_ (writeSource m dir) (symbols description)

symbols :: Description -> [Symbol]
symbols (Description _ items) = map symbol items
  where symbol (Item s _ _) = s

writeIndexJson :: FilePath -> Description -> IO ()
writeIndexJson dir description = writeJSON file description
  where file = dir </> m' <.> "json"
        Description (Module _ m') _ = description

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON file o = withFile file WriteMode handler
  where handler f = B.hPutStrLn f (A.encode o)

base64 :: String -> String
base64 = filter (not . isSpace) . B64.encode

loadDescription :: Module -> [Symbol] -> [Signature] -> IO Description
loadDescription m ss sigs = Description m <$> loadItems m ss sigs

loadItems :: Module -> [Symbol] -> [Signature] -> IO [Item]
loadItems m ss sigs = mapM (loadItem m) $ zip ss sigs

loadItem :: Module -> (Symbol, Signature) -> IO Item
loadItem m (s, sig) = Item s sig . Source . base64 . intercalate "\n" <$> source m s

instance ToJSON Description where
  toJSON (Description (Module _ m) items) = object [ L.pack m .= object (map toPair items) ]
    where toPair (Item (Symbol s) (Signature sig) (Source src64)) =
              L.pack s .=
                object [ "qname"        .= qname s
                       , "signature"    .= sig
                       , "sourceFile"   .= sourceFile s
                       , "sourceBase64" .= src64
                       ]
          qname s = m ++ "." ++ s
          sourceFile s = m ++ "_" ++ s <.> "hs"

writeSource :: Module -> FilePath -> Symbol -> IO ()
writeSource m@(Module _ m') dir s@(Symbol s') = do
  content <- unlines <$> source m s
  writeFile file content
    where file = dir </> qname <.> "hs"
          qname = m' ++ "_" ++ s'
