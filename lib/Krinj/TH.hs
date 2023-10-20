module Krinj.TH where

import Data.Text (pack)
import Language.Haskell.TH.Syntax (Exp, lift, Q, runIO)
import System.Process (readProcess)

-- read the current git commit hash
commitHashQ :: Q Exp
commitHashQ = runIO (pack <$> readProcess "git" ["rev-parse", "HEAD"] "") >>= lift

