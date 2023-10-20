{-# LANGUAGE TemplateHaskell #-}
module Krinj.BuildUtils where

import Krinj.TH (commitHashQ)
import Data.Text (Text)

-- the commit hash at the time the executable was built
commitHash :: Text
commitHash = $(commitHashQ)

