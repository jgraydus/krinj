module IssueTracker.App.GenerateJsBindings where

import IssueTracker.Lib.GenerateJsBindings
import System.Exit (exitSuccess)

main :: IO ()
main = do
  putStrLn "generating javascript bindings for http endpoints"
  writeJSCode
  putStrLn "DONE"
  exitSuccess

