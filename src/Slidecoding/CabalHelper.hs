module Slidecoding.CabalHelper
    ( loadExposedModules
    ) where

import Slidecoding.Types

import Data.List        (intercalate, isSuffixOf, uncons)
import System.Directory (getDirectoryContents)
import System.FilePath  ((</>))

-- Cabal
import Distribution.ModuleName                       as C (ModuleName)
import Distribution.ModuleName                            (components)
import Distribution.PackageDescription                    (GenericPackageDescription, exposedModules, library)
import Distribution.PackageDescription.Configuration      (flattenPackageDescription)
import Distribution.PackageDescription.Parse              (readPackageDescription)
import Distribution.Verbosity                             (normal)

loadExposedModules :: FilePath -> IO (Maybe [Module])
loadExposedModules directory = do
  cabalFile <- locateCabalFile directory
  case cabalFile of
    Just file -> toModules directory . extractModuleNames <$> loadCabal file
    Nothing   -> return Nothing

locateCabalFile :: FilePath -> IO (Maybe FilePath)
locateCabalFile directory = fmap (directory </>) . pick <$> listFiles (suffixedBy "cabal") directory

pick :: [a] -> Maybe a
pick list = fst <$> uncons list

suffixedBy :: String -> String -> Bool
suffixedBy = isSuffixOf

listFiles :: (String -> Bool) -> FilePath -> IO [FilePath]
listFiles predicate path = filter predicate <$> getDirectoryContents path

loadCabal :: FilePath -> IO GenericPackageDescription
loadCabal = readPackageDescription normal

extractModuleNames :: GenericPackageDescription -> Maybe [C.ModuleName]
extractModuleNames = fmap exposedModules . library . flattenPackageDescription

toModules :: FilePath -> Maybe [C.ModuleName] -> Maybe [Module]
toModules directory = fmap (map (convertModuleName directory))

convertModuleName :: FilePath -> C.ModuleName -> Module
convertModuleName dir mn = Module dir $ intercalate "." (components mn)
