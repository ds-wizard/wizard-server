module Shared.Api.Handler.Root.Api where

import Servant

import Shared.Api.Handler.Root.List_GET
import Shared.Model.Context.ServerContext

type RootAPI = List_GET

rootApi :: Proxy RootAPI
rootApi = Proxy

rootServer :: ServerContextC s sc m => String -> ServerT RootAPI m
rootServer = list_GET
