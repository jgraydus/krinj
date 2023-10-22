{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Data.Aeson (Value)
import Control.Monad.Reader (runReaderT)
import Data.Either (fromRight, isLeft, isRight)
import Data.Int (Int64)
import Data.List (sort)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Pool
import Data.Set qualified as Set
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

      -----------------------------------------------------------------------------------
      describe "entity types" $ do

        it "creates an entity type" $
          withTestContext $ \ctx -> do
            let TestContext { databaseConnectionPool } = ctx

            Right project <- flip runReaderT ctx $ createProject "name" "description"
            result <- flip runReaderT ctx $
              createEntityType project.projectId "entity type name" "entity type descriptor"

            -- verify the return value is correct
            result `shouldSatisfy` isRight
            let Right entityType = result
            entityType.projectId `shouldBe` project.projectId
            entityType.name `shouldBe` "entity type name"
            entityType.descriptor `shouldBe` "entity type descriptor"

            -- verify that the entity type is in the database
            record :: Maybe (Text, Value) <- fmap listToMaybe $
              withResource databaseConnectionPool $ \conn ->
                query conn [sql|SELECT name, descriptor
                                FROM entity_types
                                WHERE entity_type_id = ?|]
                      (Only entityType.entityTypeId)
            record `shouldBe` Just ("entity type name", "entity type descriptor")

        it "updates entity type name" $
          withTestContext $ \ctx -> do
            let TestContext { databaseConnectionPool } = ctx

            Right entityType <- flip runReaderT ctx $ do
              Right project <- createProject "name" "description"
              createEntityType project.projectId "entity type name" "entity type descriptor"

            result <- flip runReaderT ctx $
              updateEntityType entityType.entityTypeId (Just "new name") Nothing

            -- verify that the result value is correct
            result `shouldSatisfy` isRight
            let Right afterUpdate = result
            afterUpdate.name `shouldBe` "new name"
            afterUpdate.descriptor `shouldBe` "entity type descriptor"

            -- verify that the database was updated
            record :: Maybe (Text, Value) <- fmap listToMaybe $
              withResource databaseConnectionPool $ \conn ->
                query conn [sql|SELECT name, descriptor
                                FROM entity_types
                                WHERE entity_type_id = ?|]
                      (Only entityType.entityTypeId)
            record `shouldBe` Just ("new name", "entity type descriptor")

        it "updates entity type descriptor" $
          withTestContext $ \ctx -> do
            let TestContext { databaseConnectionPool } = ctx

            Right entityType <- flip runReaderT ctx $ do
              Right project <- createProject "name" "description"
              createEntityType project.projectId "entity type name" "entity type descriptor"

            result <- flip runReaderT ctx $
              updateEntityType entityType.entityTypeId Nothing (Just "new descriptor")

            -- verify that the result value is correct
            result `shouldSatisfy` isRight
            let Right afterUpdate = result
            afterUpdate.name `shouldBe` "entity type name"
            afterUpdate.descriptor `shouldBe` "new descriptor"

            -- verify that the database was updated
            record :: Maybe (Text, Value) <- fmap listToMaybe $
              withResource databaseConnectionPool $ \conn ->
                query conn [sql|SELECT name, descriptor
                                FROM entity_types
                                WHERE entity_type_id = ?|]
                      (Only entityType.entityTypeId)
            record `shouldBe` Just ("entity type name", "new descriptor")

        it "updates entity type name and descriptor" $
          withTestContext $ \ctx -> do
            let TestContext { databaseConnectionPool } = ctx

            Right entityType <- flip runReaderT ctx $ do
              Right project <- createProject "name" "description"
              createEntityType project.projectId "entity type name" "entity type descriptor"

            result <- flip runReaderT ctx $
              updateEntityType entityType.entityTypeId (Just "new name") (Just "new descriptor")

            -- verify that the result value is correct
            result `shouldSatisfy` isRight
            let Right afterUpdate = result
            afterUpdate.name `shouldBe` "new name"
            afterUpdate.descriptor `shouldBe` "new descriptor"

            -- verify that the database was updated
            record :: Maybe (Text, Value) <- fmap listToMaybe $
              withResource databaseConnectionPool $ \conn ->
                query conn [sql|SELECT name, descriptor
                                FROM entity_types
                                WHERE entity_type_id = ?|]
                      (Only entityType.entityTypeId)
            record `shouldBe` Just ("new name", "new descriptor")

        it "deletes an entity type" $
          withTestContext $ \ctx -> do
            let TestContext { databaseConnectionPool } = ctx

            Right entityType <- flip runReaderT ctx $ do
              Right project <- createProject "name" "description"
              createEntityType project.projectId "entity type name" "entity type descriptor"

            Just (Only countBeforeDelete) :: Maybe (Only Int64) <- fmap listToMaybe $
              withResource databaseConnectionPool $ \conn ->
                query conn [sql|SELECT count(*)
                                FROM entity_types
                                WHERE entity_type_id = ?|]
                      (Only entityType.entityTypeId)
            countBeforeDelete `shouldBe` 1

            result <- flip runReaderT ctx $ deleteEntityType entityType.entityTypeId
            result `shouldSatisfy` isRight

            Just (Only countAfterDelete) :: Maybe (Only Int64) <- fmap listToMaybe $
              withResource databaseConnectionPool $ \conn ->
                query conn [sql|SELECT count(*)
                                FROM entity_types
                                WHERE entity_type_id = ?|]
                      (Only entityType.entityTypeId)
            countAfterDelete `shouldBe` 0

        it "finds an entity type" $
          withTestContext $ \ctx -> do

            Right entityType <- flip runReaderT ctx $ do
              Right project <- createProject "name" "description"
              createEntityType project.projectId "entity type name" "entity type descriptor"

            result <- flip runReaderT ctx $ getEntityType entityType.entityTypeId

            result `shouldSatisfy` isRight
            let Right entityType' = result
            entityType'.projectId `shouldBe` entityType.projectId
            entityType'.name `shouldBe` entityType.name
            entityType'.descriptor `shouldBe` entityType.descriptor

        it "finds entity types by project ids" $
          withTestContext $ \ctx -> do
            let toSet = Set.fromList . fmap (\x -> x.entityTypeId)

            (project1Id, project2Id, expected) <- flip runReaderT ctx $ do
              Right project1 <- createProject "name" "description"
              Right entityType1 <- createEntityType project1.projectId "entity type name1" "entity type descriptor"
              Right entityType2 <- createEntityType project1.projectId "entity type name2" "entity type descriptor"
              Right entityType3 <- createEntityType project1.projectId "entity type name3" "entity type descriptor"
              Right project2 <- createProject "name" "description"
              Right entityType4 <- createEntityType project2.projectId "entity type name1" "entity type descriptor"
              Right entityType5 <- createEntityType project2.projectId "entity type name2" "entity type descriptor"
              Right entityType6 <- createEntityType project2.projectId "entity type name3" "entity type descriptor"
              let expected = Map.fromList
                             [ (project1.projectId, toSet [entityType1, entityType2, entityType3])
                             , (project2.projectId, toSet [entityType4, entityType5, entityType6])
                             ]
              pure (project1.projectId, project2.projectId, expected)

            result <- flip runReaderT ctx $ getEntityTypes [project1Id, project2Id]

            result `shouldSatisfy` isRight
            let Right tmp = result
                actual = Map.map toSet tmp

            actual `shouldBe` expected

      -----------------------------------------------------------------------------------
      describe "entities" $ do

        it "creates an entity" $
          withTestContext $ \ctx -> do
            let TestContext { databaseConnectionPool } = ctx

            (projectId, entityTypeId) <- flip runReaderT ctx $ do
              Right project <- createProject "name" "description"
              Right entityType <- createEntityType project.projectId "name" "descriptor"
              pure (project.projectId, entityType.entityTypeId)

            result <- flip runReaderT ctx $ createEntity projectId entityTypeId

            result `shouldSatisfy` isRight
            let Right entity = result
            entity.projectId `shouldBe` projectId

            -- verify entity was written to database
            record :: Maybe (ProjectId, EntityTypeId) <- fmap listToMaybe $
              withResource databaseConnectionPool $ \conn ->
                query conn [sql|SELECT project_id, entity_type_id
                                FROM entities
                                WHERE entity_id = ?|]
                      (Only entity.entityId)
            record `shouldBe` Just (projectId, entityTypeId)

        it "updates an entity's entity type id" $
          withTestContext $ \ctx -> do
            let TestContext { databaseConnectionPool } = ctx

            (entity, entityType2) <- flip runReaderT ctx $ do
              Right project <- createProject "name" "description"
              Right entityType1 <- createEntityType project.projectId "name1" "descriptor"
              Right entityType2 <- createEntityType project.projectId "name2" "descriptor"
              Right entity <- createEntity project.projectId entityType1.entityTypeId
              pure (entity, entityType2)

            result <- flip runReaderT ctx $
                        updateEntity entity.entityId
                                     Nothing
                                     (Just entityType2.entityTypeId)

            result `shouldSatisfy` isRight

            let Right entityAfterUpdate = result
            entityAfterUpdate.projectId `shouldBe` entity.projectId
            entityAfterUpdate.entityTypeId `shouldBe` entityType2.entityTypeId

            -- verify entity was written to database
            record :: Maybe (ProjectId, EntityTypeId) <- fmap listToMaybe $
              withResource databaseConnectionPool $ \conn ->
                query conn [sql|SELECT project_id, entity_type_id
                                FROM entities
                                WHERE entity_id = ?|]
                      (Only entity.entityId)
            record `shouldBe` Just (entity.projectId, entityType2.entityTypeId)

        it "updates an entity's project id and entity type id" $
          withTestContext $ \ctx -> do
            let TestContext { databaseConnectionPool } = ctx

            Right entity <- flip runReaderT ctx $ do
              Right project <- createProject "name" "description"
              Right entityType <- createEntityType project.projectId "name" "descriptor"
              createEntity project.projectId entityType.entityTypeId

            (projectId, entityTypeId) <- flip runReaderT ctx $ do
              Right project <- createProject "name" "description"
              Right entityType <- createEntityType project.projectId "name" "descriptor"
              pure (project.projectId, entityType.entityTypeId)

            result <- flip runReaderT ctx $ updateEntity entity.entityId (Just projectId) (Just entityTypeId)

            result `shouldSatisfy` isRight

            let Right entityAfterUpdate = result
            entityAfterUpdate.projectId `shouldBe` projectId
            entityAfterUpdate.entityTypeId `shouldBe` entityTypeId

            -- verify entity was written to database
            record :: Maybe (ProjectId, EntityTypeId) <- fmap listToMaybe $
              withResource databaseConnectionPool $ \conn ->
                query conn [sql|SELECT project_id, entity_type_id
                                FROM entities
                                WHERE entity_id = ?|]
                      (Only entity.entityId)
            record `shouldBe` Just (projectId, entityTypeId)

        describe "does NOT update entity when entity type does not belong to project" $ do
          it "when updating project id" $
            withTestContext $ \ctx -> do

              (entity, project2Id) <- flip runReaderT ctx $ do
                Right project1 <- createProject "name" "description"
                Right project2 <- createProject "name" "description"
                Right entityType <- createEntityType project1.projectId "name" "descriptor"
                Right entity <- createEntity project1.projectId entityType.entityTypeId
                pure (entity, project2.projectId)

              result <- flip runReaderT ctx $
                          updateEntity entity.entityId
                                       (Just project2Id)
                                       Nothing

              result `shouldSatisfy` isLeft

          it "when updating entity type id" $
            withTestContext $ \ctx -> do

              Right entity <- flip runReaderT ctx $ do
                Right project <- createProject "name" "description"
                Right entityType <- createEntityType project.projectId "name" "descriptor"
                createEntity project.projectId entityType.entityTypeId

              Right differentProjectEntityType  <- flip runReaderT ctx $ do
                Right project <- createProject "name" "description"
                createEntityType project.projectId "name" "descriptor"

              result <- flip runReaderT ctx $
                          updateEntity entity.entityId
                                       Nothing
                                       (Just differentProjectEntityType.entityTypeId)

              result `shouldSatisfy` isLeft

          it "when updating project id and entity type id" $
            withTestContext $ \ctx -> do

              Right entity <- flip runReaderT ctx $ do
                Right project <- createProject "name" "description"
                Right entityType <- createEntityType project.projectId "name" "descriptor"
                createEntity project.projectId entityType.entityTypeId

              (projectId, differentProjectEntityTypeId)  <- flip runReaderT ctx $ do
                Right project1 <- createProject "name" "description"
                Right project2 <- createProject "name" "description"
                Right entityType <- createEntityType project2.projectId "name" "descriptor"
                pure (project1.projectId, entityType.entityTypeId)

              result <- flip runReaderT ctx $
                          updateEntity entity.entityId
                                       (Just projectId)
                                       (Just differentProjectEntityTypeId)

              result `shouldSatisfy` isLeft

        it "deletes an entity" $
          withTestContext $ \ctx -> do
            let TestContext { databaseConnectionPool } = ctx

            Right entity <- flip runReaderT ctx $ do
              Right project <- createProject "name" "description"
              Right entityType <- createEntityType project.projectId "name" "descriptor"
              createEntity project.projectId entityType.entityTypeId

            Just (Only countBeforeDelete) :: Maybe (Only Int64) <- fmap listToMaybe $
              withResource databaseConnectionPool $ \conn ->
                query conn [sql|SELECT count(*) FROM entities WHERE entity_id = ?|]
                      (Only entity.entityId)
            countBeforeDelete `shouldBe` 1

            result <- flip runReaderT ctx $ deleteEntity entity.entityId
            result `shouldSatisfy` isRight

            Just (Only countAfterDelete) :: Maybe (Only Int64) <- fmap listToMaybe $
              withResource databaseConnectionPool $ \conn ->
                query conn [sql|SELECT count(*) FROM entities WHERE entity_id = ?|]
                      (Only entity.entityId)
            countAfterDelete `shouldBe` 0

        it "finds an entity by id" $
          withTestContext $ \ctx -> do
            Right entity <- flip runReaderT ctx $ do
              Right project <- createProject "name" "description"
              Right entityType <- createEntityType project.projectId "name" "descriptor"
              createEntity project.projectId entityType.entityTypeId

            result <- flip runReaderT ctx $ getEntity entity.entityId

            result `shouldSatisfy` isRight
            let Right entity' = result
            entity'.projectId `shouldBe` entity.projectId
            entity'.entityTypeId `shouldBe` entity.entityTypeId

        it "finds all entities for a project" $
          withTestContext $ \ctx -> do
            let toSet = Set.fromList . fmap (\x -> x.entityId)

            (projectId, expected) <- flip runReaderT ctx $ do
              Right project1 <- createProject "name" "description"
              Right entityType1 <- createEntityType project1.projectId "name1" "descriptor"
              Right entity1 <- createEntity project1.projectId entityType1.entityTypeId
              Right entity2 <- createEntity project1.projectId entityType1.entityTypeId
              Right entity3 <- createEntity project1.projectId entityType1.entityTypeId
              Right project2 <- createProject "name" "description"
              Right entityType2 <- createEntityType project2.projectId "name1" "descriptor"
              _ <- createEntity project2.projectId entityType2.entityTypeId
              _ <- createEntity project2.projectId entityType2.entityTypeId
              _ <- createEntity project2.projectId entityType2.entityTypeId
              pure (project1.projectId, toSet [entity1, entity2, entity3])

            result <- flip runReaderT ctx $ getEntities projectId 

            result `shouldSatisfy` isRight
            let Right tmp = result
                actual = toSet tmp
            actual `shouldBe` expected

{-
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
