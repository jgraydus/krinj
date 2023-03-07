module EntityService.Internal.Command.Projects (
    createProject, updateProject, deleteProject
) where

import Database.PostgreSQL.Simple (Connection)
import EntityService.Internal
import EntityService.Internal.Model
import GHC.Records (getField)
import Opaleye

-- |
createProject :: Connection -> ProjectName -> ProjectDescription -> IO (Result Project)
createProject conn name description = do
  result <- runInsert conn insert
  pure $ case result of
    project : _ -> Right project
    _           -> Left InsertFailed
  where
    insert = Insert
      { iTable = projectsTable
      , iRows = [ProjectsRowT Nothing (toFields name) (toFields description)]
      , iReturning = rReturning id
      , iOnConflict = Nothing
      }

-- |
updateProject :: Connection -> ProjectId -> Maybe ProjectName -> Maybe ProjectDescription -> IO (Result Project)
updateProject conn projectId1 nameMaybe descriptionMaybe = do
  result <- runUpdate conn update
  pure $ case result of
    project : _ -> Right project
    _           -> Left UpdateFailed
  where
    updateWith :: ProjectsRowR -> ProjectsRowR
    updateWith row@ProjectsRowT {..} = 
      ProjectsRowT
      { name        = maybe row.name toFields nameMaybe
      , description = maybe row.description toFields descriptionMaybe
      , ..
      }
    update = Update
      { uTable = projectsTable
      , uUpdateWith = updateEasy updateWith
      , uWhere = (.== toFields projectId1) . (getField @"projectId")
      , uReturning = rReturning id
      }

-- |
deleteProject :: Connection -> ProjectId -> IO (Result ())
deleteProject conn projectId = do
  deleteCount <- runDelete conn delete
  pure $ if deleteCount == 1 then Right () else Left DeleteFailed
  where
    delete = Delete
      { dTable = projectsTable
      , dWhere = (.== toFields projectId) . (getField @"projectId") 
      , dReturning = rCount
      }

