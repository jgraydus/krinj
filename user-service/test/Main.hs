{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Control.Monad.Reader (runReaderT)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Pool
import Data.Text (Text)
import Database.PostgreSQL.Simple qualified as P
import Database.PostgreSQL.Simple (query, Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Krinj.UserService
import Krinj.UserService.Types (Password(..), Salt(..), User(..))
import Krinj.Test.Util (getConnectInfo, withTestDatabase)
import Test.Hspec

data TestContext =
  TestContext
  { databaseConnectionPool :: Pool P.Connection
  , salt :: Salt
  }

main :: IO ()
main = do
  connectInfo <- getConnectInfo

  let withTestContext :: (TestContext -> IO a) -> IO a
      withTestContext action = withTestDatabase connectInfo $ \databaseConnectionPool ->
        action (TestContext { databaseConnectionPool, salt = "塩" })

  hspec $ parallel $ do

    describe "user-service tests" $ do

      -----------------------------------------------------------------------------------
      it "creates a new user" $
        withTestContext $ \ctx -> do
          let emailAddress = "user@user.com"

          result <- flip runReaderT ctx $ createUser emailAddress "password"

          result `shouldSatisfy` isJust
          let user = fromJust result
          user.emailAddress `shouldBe` emailAddress

      -----------------------------------------------------------------------------------
      it "does NOT create a user if the email address is already used" $
        withTestContext $ \ctx -> do
          let emailAddress = "user@user.com"

          result <- flip runReaderT ctx $ do
            _ <- createUser emailAddress "password"
            createUser emailAddress "passthisword"

          result `shouldSatisfy` isNothing

      -----------------------------------------------------------------------------------
      it "finds a user by userId" $
        withTestContext $ \ctx -> do
          let emailAddress = "user@user.com"

          Just (User {userId}) <- flip runReaderT ctx $ createUser emailAddress "password"
          result <- flip runReaderT ctx $ findUserById userId

          result `shouldSatisfy` isJust
          let user = fromJust result
          user.userId `shouldBe` userId
          user.emailAddress `shouldBe` emailAddress

      -----------------------------------------------------------------------------------
      it "finds a user by credentials" $
        withTestContext $ \ctx -> do
          let emailAddress = "user@user.com"
              password = "password"

          Just (User {userId}) <- flip runReaderT ctx $ createUser emailAddress password
          result <- flip runReaderT ctx $ findUserByCredentials emailAddress password

          result `shouldSatisfy` isJust
          let user = fromJust result
          user.userId `shouldBe` userId
          user.emailAddress `shouldBe` emailAddress

      -----------------------------------------------------------------------------------
      it "does NOT find a user by credentials if credentials are wrong" $
        withTestContext $ \ctx -> do
          let emailAddress = "user@user.com"
              password = "password"

          _ <- flip runReaderT ctx $ createUser emailAddress password
          result <- flip runReaderT ctx $ findUserByCredentials emailAddress "wrong password"

          result `shouldSatisfy` isNothing

      -----------------------------------------------------------------------------------
      it "hashes the password before saving credentials" $ do
        withTestContext $ \ctx -> do
          let TestContext { databaseConnectionPool } = ctx
              emailAddress = "user@user.com"
              password = "password"

          Just (User {userId}) <- flip runReaderT ctx $ createUser emailAddress password

          [(Only passwordHash)] :: [Only Text] <- withResource databaseConnectionPool $ \conn ->
            query conn [sql|SELECT password_hash FROM credentials WHERE user_id = ?|] (Only userId)

          Password passwordHash `shouldNotBe` password

