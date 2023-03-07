{-# LANGUAGE TemplateHaskell #-}
module IssueTracker.BuildUtils where

import IssueTracker.TH (commitHashQ)
import Data.Text (Text)

-- the commit hash at the time the executable was built
commitHash :: Text
commitHash = $(commitHashQ)

