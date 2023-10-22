{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Control.Monad.Reader (runReaderT)
import Data.Either (fromRight, isRight)
import Data.Int (Int64)
import Data.List (sort)
import Data.Maybe (listToMaybe)
import Data.Pool
import Data.Text (Text)
import Database.PostgreSQL.Simple qualified as P
import Database.PostgreSQL.Simple (query, Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Krinj.Config (ApplicationConfig, readConfig)
import Krinj.EntityService
import Krinj.Test.Util (getConnectInfo, withTestDatabase)
import Test.Hspec

data TestContext =
  TestContext
  { databaseConnectionPool :: Pool P.Connection
  , applicationConfig :: ApplicationConfig
  }

main :: IO ()
main = do
  applicationConfig <- fromRight (error "failed to read config") <$> readConfig "../config" "localhost"
  connectInfo <- getConnectInfo

  let withTestContext :: (TestContext -> IO a) -> IO a
      withTestContext action = withTestDatabase connectInfo $ \databaseConnectionPool ->
        action (TestContext { databaseConnectionPool, applicationConfig })

  hspec $ parallel $ do
    describe "entity-service" $ do

      -----------------------------------------------------------------------------------
      describe "projects" $ do
        it "creates a new project" $
          withTestContext $ \ctx -> do
            let TestContext { databaseConnectionPool } = ctx

            result <- flip runReaderT ctx $ createProject "name" "description"

            -- verify that the returned object is correct
            result `shouldSatisfy` isRight
            let Right project = result
            project.name `shouldBe` "name"
            project.description `shouldBe` "description"

            -- verify that the project is ine the database
            record :: Maybe (Text, Text) <- fmap listToMaybe $
              withResource databaseConnectionPool $ \conn ->
                query conn [sql|SELECT name, description FROM projects WHERE project_id = ?|]
                      (Only project.projectId)
            record `shouldBe` Just ("name", "description")

        it "updates a project name" $
          withTestContext $ \ctx -> do
            let TestContext { databaseConnectionPool } = ctx

            result <- flip runReaderT ctx $ do
              Right project <- createProject "name" "description"
              updateProject project.projectId (Just "new name") Nothing

            -- verify that the returned object is correct
            result `shouldSatisfy` isRight
            let Right project = result
            project.name `shouldBe` "new name"
            project.description `shouldBe` "description"

            -- verify that the database was updated
            record :: Maybe (Text, Text) <- fmap listToMaybe $
              withResource databaseConnectionPool $ \conn ->
                query conn [sql|SELECT name, description FROM projects WHERE project_id = ?|]
                      (Only project.projectId)
            record `shouldBe` Just ("new name", "description")

        it "updates a project description" $
          withTestContext $ \ctx -> do
            let TestContext { databaseConnectionPool } = ctx

            result <- flip runReaderT ctx $ do
              Right project <- createProject "name" "description"
              updateProject project.projectId Nothing (Just "new description")

            -- verify that the returned object is correct
            result `shouldSatisfy` isRight
            let Right project = result
            project.name `shouldBe` "name"
            project.description `shouldBe` "new description"

            -- verify that the database was updated
            record :: Maybe (Text, Text) <- fmap listToMaybe $
              withResource databaseConnectionPool $ \conn ->
                query conn [sql|SELECT name, description FROM projects WHERE project_id = ?|]
                      (Only project.projectId)
            record `shouldBe` Just ("name", "new description")

        it "updates a project name and description" $
          withTestContext $ \ctx -> do
            let TestContext { databaseConnectionPool } = ctx

            result <- flip runReaderT ctx $ do
              Right project <- createProject "name" "description"
              updateProject project.projectId (Just "new name") (Just "new description")

            -- verify that the returned object is correct
            result `shouldSatisfy` isRight
            let Right project = result
            project.name `shouldBe` "new name"
            project.description `shouldBe` "new description"

            -- verify that the database was updated
            record :: Maybe (Text, Text) <- fmap listToMaybe $
              withResource databaseConnectionPool $ \conn ->
                query conn [sql|SELECT name, description FROM projects WHERE project_id = ?|]
                      (Only project.projectId)
            record `shouldBe` Just ("new name", "new description")

        it "deletes a project" $
          withTestContext $ \ctx -> do
            let TestContext { databaseConnectionPool } = ctx

            Right project <- flip runReaderT ctx $ createProject "name" "description"

            Just (Only countBeforeDelete) :: Maybe (Only Int64) <- fmap listToMaybe $
              withResource databaseConnectionPool $ \conn ->
                query conn [sql|SELECT count(*) FROM projects WHERE project_id = ?|]
                      (Only project.projectId)
            countBeforeDelete `shouldBe` 1

            result <- flip runReaderT ctx $ deleteProject project.projectId
            result `shouldSatisfy` isRight

            Just (Only countAfterDelete) :: Maybe (Only Int64) <- fmap listToMaybe $
              withResource databaseConnectionPool $ \conn ->
                query conn [sql|SELECT count(*) FROM projects WHERE project_id = ?|]
                      (Only project.projectId)
            countAfterDelete `shouldBe` 0

        it "finds a project by project id" $
          withTestContext $ \ctx -> do
            Right project <- flip runReaderT ctx $ createProject "name" "description"
            result <- flip runReaderT ctx $ getProject project.projectId
            result `shouldSatisfy` isRight
            let Right project' = result
            project'.name `shouldBe` "name"
            project'.description `shouldBe` "description"

        it "finds multiple projects" $
          withTestContext $ \ctx -> do
            flip runReaderT ctx $ do
              _ <- createProject "name1" "description"
              _ <- createProject "name2" "description"
              _ <- createProject "name3" "description"
              _ <- createProject "name4" "description"
              _ <- createProject "name5" "description"
              pure ()

            result <- flip runReaderT ctx $ getProjects (error "paging not implemented yet")

            result `shouldSatisfy` isRight
            let Right projects = result
            sort (fmap (\x -> x.name) projects) `shouldBe` ["name1", "name2", "name3", "name4", "name5"]


{-
  createEntityType :: ProjectId -> EntityTypeName -> EntityTypeDescriptor -> m (Result EntityType)
  updateEntityType :: EntityTypeId -> Maybe EntityTypeName -> Maybe EntityTypeDescriptor -> m (Result EntityType)
  deleteEntityType :: EntityTypeId -> m (Result ())
  getEntityType    :: EntityTypeId -> m (Result EntityType)
  getEntityTypes   :: [ProjectId] -> m (Result (Map ProjectId [EntityType]))

  createEntity :: ProjectId -> EntityTypeId -> m (Result Entity)
  updateEntity :: EntityId -> Maybe ProjectId -> Maybe EntityTypeId -> m (Result Entity)
  deleteEntity :: EntityId -> m (Result ())
  getEntity    :: EntityId -> m (Result Entity)
  getEntities  :: ProjectId -> m (Result [Entity])

  createAttributes :: EntityId -> [(AttributeName, AttributeValue)] -> m (Result [Attribute])
  updateAttribute  :: EntityId -> AttributeName -> AttributeValue -> m (Result Attribute)
  deleteAttribute  :: EntityId -> AttributeName -> m (Result ())
  getAttribute     :: EntityId -> AttributeName -> m (Result Attribute)
  getAttributes    :: [EntityId] -> m (Result (Map EntityId [Attribute]))

  createRelationship :: EntityId -> EntityId -> RelationshipType -> m (Result Relationship)
  deleteRelationship :: RelationshipId -> m (Result ())
  getRelationship :: RelationshipId -> m (Result Relationship)
  getRelationships :: EntityId -> m (Result [Relationship])
-}
