module Main where

import DevServer (defaultDevServerConfig, DevServerConfig(..), runDevServer)

main :: IO ()
main = do
  let config = defaultDevServerConfig { serverExeName = "web", clientFileExtensions = ["ts", "tsx", "js"] }
  runDevServer config

