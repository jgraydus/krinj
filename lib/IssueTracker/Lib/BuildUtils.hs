{-# LANGUAGE TemplateHaskell #-}
module IssueTracker.Lib.BuildUtils where

import           Data.Text (Text)
import           IssueTracker.Lib.TH (commitHashQ)

-- the commit hash at the time the executable was built
commitHash :: Text
commitHash = $(commitHashQ)

