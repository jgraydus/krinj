module IssueTracker.Web.Routes where

import IssueTracker.Web.Routes.Attributes
import IssueTracker.Web.Routes.Entities
import IssueTracker.Web.Routes.EntityTypes
import IssueTracker.Web.Routes.Projects
import IssueTracker.Web.Routes.Site
import IssueTracker.Web.RouteHandler
import Servant

type API = "api" :> (
       AttributesApi
  :<|> EntitiesApi
  :<|> EntityTypesApi
  :<|> ProjectsApi
  )

apiHandler :: RouteHandler API
apiHandler =
       attributesApiHandler
  :<|> entitiesApiHandler
  :<|> entityTypesApiHandler
  :<|> projectsApiHandler

type APIAndSite = API :<|> Site

apiAndSiteHandler :: RouteHandler APIAndSite
apiAndSiteHandler = apiHandler :<|> siteHandler

