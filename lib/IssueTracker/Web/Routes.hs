module IssueTracker.Web.Routes where

import IssueTracker.Web.Routes.Projects
import IssueTracker.Web.Routes.Site
import IssueTracker.Web.RouteHandler
import Servant

type API = "api" :> ProjectsAPI

apiHandler :: RouteHandler API
apiHandler = projectsAPIHandler

type APIAndSite = API :<|> Site

apiAndSiteHandler :: RouteHandler APIAndSite
apiAndSiteHandler = apiHandler :<|> siteHandler

