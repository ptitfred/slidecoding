module Slidecoding
    (
    -- GHCI
      run
    , ioStream

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
    , ModuleName
    , Name
    , Port
    , Stream(..)
    , singleModuleContext

    -- WebSockets
    , start
    ) where

import Slidecoding.GHCI
import Slidecoding.Presentation
import Slidecoding.ReplSession
import Slidecoding.Types
import Slidecoding.WebSockets
