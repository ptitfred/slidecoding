module Slidecoding.WebSockets
    ( start
    ) where

import Slidecoding.GHCI  (run)
import Slidecoding.Types (Port, Presentation, Stream(..))

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Network.WebSockets         as WS

start :: Port -> Presentation -> IO ()
start port p = WS.runServer "0.0.0.0" port $ application p

wsStream :: WS.Connection -> Stream IO
wsStream conn = Stream nothing input output
  where nothing = return ()
        input   = C.unpack <$> WS.receiveData conn
        output  = WS.sendTextData conn . C.pack

application :: Presentation -> WS.ServerApp
application p pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  run (wsStream conn) p
