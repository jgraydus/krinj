{-# LANGUAGE TemplateHaskell #-}
module Issues.Lib.BuildUtils where

import Data.Text (Text)
import Issues.Lib.TH (commitHashQ)

-- the commit hash at the time the executable was built
commitHash :: Text
commitHash = $(commitHashQ)

