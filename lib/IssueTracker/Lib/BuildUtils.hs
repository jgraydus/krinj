{-# LANGUAGE TemplateHaskell #-}
module IssueTracker.Lib.BuildUtils where

import IssueTracker.Lib.TH (commitHashQ)
import Data.Text (Text)

-- the commit hash at the time the executable was built
commitHash :: Text
commitHash = $(commitHashQ)

