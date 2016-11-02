{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Slidecoding.Types
    ( Context(..)
    , Description(..)
    , Design(..)
    , Item(..)
    , Metadata(..)
    , Module(..)
    , ModuleName
    , Name
    , Pixel
    , Port
    , Presentation(..)
    , Signature(..)
    , Source(..)
    , Stream(..)
    , Symbol(..)
    , Theme(..)
    , ValidationMessage
    , singleModuleContext
    ) where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Foldable    (asum)
import System.FilePath  ((</>), (<.>))

type Port = Int
type Name = String
type ModuleName = String

data Module       = Module FilePath ModuleName
newtype Symbol    = Symbol String
newtype Signature = Signature String

data Description = Description Module [Item]
data Item = Item Symbol Signature Source
newtype Source = Source String

singleModuleContext :: ModuleName -> Context
singleModuleContext moduleName = Context moduleName [path] [moduleName] []
  where path = "src" </> moduleName <.> "hs"

data Stream m where
  Stream :: Monad m => m ()             -- prepare
                    -> m String         -- readInput
                    -> (String -> m ()) -- writeOutput
                    -> Stream m

data Context = Context { name            :: Name         -- A name for debugging
                       , sources         :: [FilePath]   -- Source files to load
                       , modules         :: [ModuleName] -- Modules to import
                       , topLevelModules :: [ModuleName] -- Modules to interpret (private elems will be available)
                       }

data Presentation = Presentation { rootDir  :: FilePath
                                 , distDir  :: FilePath
                                 , metadata :: Metadata
                                 } deriving Show

data Metadata = Metadata { title  :: String
                         , design :: Design
                         } deriving Show

data Design = Design { width  :: Pixel
                     , height :: Pixel
                     , theme  :: Maybe Theme
                     , icon   :: Maybe FilePath
                     } deriving Show

type Pixel = Int
data Theme = Builtin String | Custom FilePath | Patch Theme FilePath deriving Show

instance FromJSON Metadata where
  parseJSON (Object v) = Metadata <$> v .: "title"
                                  <*> v .: "design"
  parseJSON invalid    = typeMismatch "Metadata" invalid

instance FromJSON Design where
  parseJSON (Object v) = Design <$> v .:? "width"  .!= 1600
                                <*> v .:? "height" .!= 1000
                                <*> v .:? "theme"
                                <*> v .:? "icon"
  parseJSON invalid    = typeMismatch "Design" invalid

instance FromJSON Theme where
  parseJSON (Object v) =
    asum [
      Patch  <$> getBase <*> getCustom,
      Custom <$> getCustom,
      getBase
    ]
      where getBase   = Builtin <$> v .: "base"
            getCustom = v .: "custom"
  parseJSON invalid    = typeMismatch "Theme" invalid

type ValidationMessage = String
