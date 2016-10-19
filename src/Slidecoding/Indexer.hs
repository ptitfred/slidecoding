{-# LANGUAGE OverloadedStrings #-}

module Slidecoding.Indexer
    ( indexIO
    ) where

import Slidecoding.Browser
import Slidecoding.Types

import Codec.Binary.Base64.String as B64 (decode)
import Data.Aeson                 hiding (encode)
import Data.Aeson                 as A   (encode)
import Data.ByteString.Lazy.Char8 as B   (hPutStrLn)
import Data.Text                  as L   (pack)

import System.Directory                  (createDirectoryIfMissing)
import System.FilePath                   ((</>), (<.>))
import System.IO                         (IOMode(..), withFile)

indexIO :: Module -> FilePath -> IO ()
indexIO m dir = do
  createDirectoryIfMissing True dir
  description <- browse m
  writeIndexJson dir description
  writeSources dir description

writeIndexJson :: FilePath -> Description -> IO ()
writeIndexJson dir description = writeJSON file (D' description)
  where file = dir </> m' <.> "json"
        Description (Module _ m') _ = description

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON file o = withFile file WriteMode handler
  where handler f = B.hPutStrLn f (A.encode o)

newtype Description' = D' Description

instance ToJSON Description' where
  toJSON (D' (Description (Module _ m) items)) = object [ L.pack m .= object (map toPair items) ]
    where toPair (Item (Symbol s) (Signature sig) (Source src64)) =
              L.pack s .=
                object [ "qname"        .= qname s
                       , "signature"    .= sig
                       , "sourceFile"   .= sourceFile s
                       , "sourceBase64" .= src64
                       ]
          qname s = m ++ "." ++ s
          sourceFile s = m ++ "_" ++ s <.> "hs"

writeSources :: FilePath -> Description -> IO ()
writeSources dir (Description m items) = mapM_ (writeItem dir m) items

writeItem :: FilePath -> Module -> Item -> IO ()
writeItem dir (Module _ m) (Item (Symbol s) _ (Source src)) =
  writeFile file (B64.decode src)
    where file = dir </> qname <.> "hs"
          qname = m ++ "_" ++ s
