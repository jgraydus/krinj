module Krinj.Web.Routes where

import Krinj.Web.Routes.Authentication
import Krinj.Web.Routes.Attributes
import Krinj.Web.Routes.Entities
import Krinj.Web.Routes.EntityTypes
import Krinj.Web.Routes.Projects
import Krinj.Web.Routes.Site
import Krinj.Web.RouteHandler
import Servant

type API = "api" :> (
       AttributesApi
  :<|> EntitiesApi
  :<|> EntityTypesApi
  :<|> ProjectsApi
  :<|> AuthenticationApi
  )

apiHandler :: RouteHandler API
apiHandler =
       attributesApiHandler
  :<|> entitiesApiHandler
  :<|> entityTypesApiHandler
  :<|> projectsApiHandler
  :<|> authenticationApiHandler

type APIAndSite = API :<|> Site

apiAndSiteHandler :: RouteHandler APIAndSite
apiAndSiteHandler = apiHandler :<|> siteHandler

