module Slidecoding.WebSockets
    ( start
    ) where

import Slidecoding.GHCI  (run)
import Slidecoding.Types (Port, Context, Stream(..))

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Network.WebSockets         as WS

start :: Port -> Context -> IO ()
start port ctx = WS.runServer "0.0.0.0" port $ application ctx

wsStream :: WS.Connection -> Stream IO
wsStream conn = Stream nothing input output
  where nothing = return ()
        input   = C.unpack <$> WS.receiveData conn
        output  = WS.sendTextData conn . C.pack

application :: Context -> WS.ServerApp
application ctx pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  run (wsStream conn) ctx
