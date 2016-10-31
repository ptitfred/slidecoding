{-# LANGUAGE GADTs #-}

module Slidecoding.Types
    ( Context(..)
    , Description(..)
    , Item(..)
    , Metadata(..)
    , Module(..)
    , ModuleName
    , Name
    , Port
    , Presentation(..)
    , Signature(..)
    , Source(..)
    , Stream(..)
    , Symbol(..)
    , ValidationMessage
    , singleModuleContext
    ) where

import Data.Text       (Text)
import System.FilePath ((</>), (<.>))

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

data Presentation = Presentation { rootDir :: FilePath
                                 , meta    :: Metadata
                                 } deriving Show

data Metadata = Metadata { title :: Text
                         } deriving Show

type ValidationMessage = String
