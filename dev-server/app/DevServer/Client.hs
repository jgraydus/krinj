module DevServer.Client where

import Control.Concurrent
import Control.Exception (finally)
import Control.Monad (forever, forM_, when)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.IORef
import Data.List (intercalate)
import Data.Maybe (isJust)
import Data.Text (Text)
import DevServer.Logger
import Network.WebSockets
import System.FSNotify
import System.Process.Typed

endsWith :: String -> String -> Bool
endsWith pattern str = pattern == drop (length str - length pattern) str

isTriggerFileType :: String -> Bool
isTriggerFileType s = endsWith ".tsx" s || endsWith ".ts" s || endsWith ".js" s

dropLastCharacter :: String -> String
dropLastCharacter s = take l s where l = length s - 1

watchClient :: FastLogger -> IO ()
watchClient log = do
  fileChangedRef :: IORef Bool <- newIORef False
  browser :: MVar Connection   <- newEmptyMVar

  let buildClient = do
        log "rebuilding client"
        writeIORef fileChangedRef False
        runProcess $ setWorkingDir "./client" $ shell "npm run build"

  let notifyBrowser = do
        log "notifying connection"
        conn <- tryTakeMVar browser
        case conn of
          Just c -> do
            log "sending RELOAD instruction"
            sendTextData @Text c "RELOAD"
          Nothing -> log "no browser connection currently"

  log "building client"
  _ <- buildClient
  log "watching for changes to client"

  -- start websocket server to notify client that it needs to reload
  forkIO $ do
    runServer "localhost" 8082 $ \pending -> do
      conn <- acceptRequest pending
      log "CONNECTION ACCEPTED"
      tryTakeMVar browser
      putMVar browser conn
      withPingThread conn 30 (pure ()) $ forever $ do
        msg <- receive conn
        sendTextData @Text conn "NOP"

  -- watch for changes
  forkIO $ withManager $ \mgr -> do
    let filter = \case
          Added path _ _ -> isTriggerFileType path
          Removed path _ _ -> isTriggerFileType path
          Unknown {} -> False
          Modified path _ _ -> isTriggerFileType path
        action _ = writeIORef fileChangedRef True
    watchTree mgr "./client/src" filter action
    forever $ threadDelay 1000000

  -- rebuild / restart server on changes
  forever $ do
    fileChanged <- readIORef fileChangedRef
    when fileChanged $ do
      log ">>> a file change has been detected <<<"
      ec <- buildClient
      when (ec == ExitSuccess) notifyBrowser
    threadDelay 2000000

