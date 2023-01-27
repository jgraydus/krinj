module Main where

import DevServer.Client
import DevServer.Server
import DevServer.Logger
import Control.Concurrent.Async (concurrently)

main :: IO ()
main = withLogger $ \log -> do

  concurrently 
    (watchServer (changeColor Green log))
    (watchClient (changeColor Red log))

  log "DONE"

