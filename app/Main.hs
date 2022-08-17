module Main where

import           Application
import           Data.Text (Text)
import           GenerateJsBindings
import qualified Network.Wai.Handler.Warp as Warp
import           System.Environment

port :: Int
port = 8080

main :: IO ()
main = do
  args <- getArgs
  if elem "--generate-bindings" args
  then do
    writeJSCode
    putStrLn "javascript bindings generated"
  else do
    putStrLn $ "server listening on port " <> (show port)
    Warp.run port app

