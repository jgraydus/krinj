module Main where

import           Application
import           Data.Text (Text)
import qualified Network.Wai.Handler.Warp as Warp

port :: Int
port = 8080

main :: IO ()
main = do
  putStrLn $ "server listening on port " <> (show port)
  Warp.run port app

