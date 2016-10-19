module Slidecoding
    (
    -- Browser
      browse

    -- CabalHelper
    , loadExposedModules

    -- GHCI
    , run
    , ioStream

    -- Indexer
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
    , Description(..)
    , Item(..)
    , Module(..)
    , ModuleName
    , Name
    , Port
    , Source(..)
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
