module Slidecoding
    (
    -- Browser
      browseSignatures
    , browseSymbols
    , source

    -- CabalHelper
    , loadExposedModules

    -- GHCI
    , run
    , ioStream

    -- Indexer
    , Description(..)
    , Source(..)
    , Item(..)
    , index
    , indexIO

    -- Presentation
    , Presentation(..)
    , Metadata(..)
    , ValidationMessage
    , load

    -- ReplSession
    , ReplSession(..)
    , evalInSession
    , startSession
    , endSession

    -- Types
    , Context(..)
    , Module(..)
    , ModuleName
    , Name
    , Port
    , Stream(..)
    , Symbol(..)
    , singleModuleContext

    -- WebSockets
    , start
    ) where

import Slidecoding.Browser
import Slidecoding.CabalHelper
import Slidecoding.GHCI
import Slidecoding.Indexer
import Slidecoding.Presentation
import Slidecoding.ReplSession
import Slidecoding.Types
import Slidecoding.WebSockets
