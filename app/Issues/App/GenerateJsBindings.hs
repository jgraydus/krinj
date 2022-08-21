module Issues.App.GenerateJsBindings where

import           System.Exit (ExitCode(..), exitWith)
import           Issues.Lib.GenerateJsBindings

main :: IO ()
main = do
  putStrLn "generating javascript bindings for http endpoints"
  writeJSCode
  putStrLn "DONE"
  exitWith ExitSuccess

