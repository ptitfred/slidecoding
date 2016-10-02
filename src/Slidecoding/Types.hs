{-# LANGUAGE GADTs #-}

module Slidecoding.Types
    ( Context(..)
    , Module(..)
    , ModuleName
    , Name
    , Port
    , Signature(..)
    , Stream(..)
    , Symbol(..)
    , singleModuleContext
    ) where

import System.FilePath ((</>), (<.>))

type Port = Int
type Name = String
type ModuleName = String

newtype Module    = Module String
newtype Symbol    = Symbol String
newtype Signature = Signature String

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
