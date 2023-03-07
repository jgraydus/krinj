module EntityService.Internal.Query.Projects (
    getProject, getProjects
) where

import Database.PostgreSQL.Simple (Connection)
import EntityService.Internal
import EntityService.Internal.Model
import GHC.Records (getField)
import Opaleye

-- | get the project with the given projectId
getProject :: Connection -> ProjectId -> IO (Result Project)
getProject conn (ProjectId projectId) = do
  result :: [Project] <- runSelect conn select
  pure $ case result of
    project : _ -> Right project
    _           -> Left NotFound 
  where
    select = do
      row@(ProjectsRowT projectId1 _ _) <- selectTable projectsTable
      where_ $ projectId1 .== toFields projectId
      pure row

-- | get all projects (TODO paging)
getProjects   :: Connection -> Page -> IO (Result [Project])
getProjects conn page = Right <$> runSelect conn select
  where
   select = orderBy (asc $ getField @"name") (selectTable projectsTable)

