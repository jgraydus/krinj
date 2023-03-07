{-# LANGUAGE UndecidableInstances #-}
module IssueTracker.Web.Routes.Site.Bundles (
    SiteBundles(..)
) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

class Monad m => SiteBundles m where
  jsBundle :: m Text
  cssBundle :: m Text

jsBundleFilePath :: FilePath
jsBundleFilePath = "client/build/bundle.js"

cssBundleFilePath :: FilePath
cssBundleFilePath = "client/build/bundle.css"

instance (Monad m, MonadIO m) => SiteBundles m where
  jsBundle :: m Text
  jsBundle = liftIO $ decodeUtf8 <$> BS.readFile jsBundleFilePath
  
  cssBundle :: m Text
  cssBundle = liftIO $ decodeUtf8 <$> BS.readFile cssBundleFilePath

