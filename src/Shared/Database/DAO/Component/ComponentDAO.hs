module Shared.Database.DAO.Component.ComponentDAO where

import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.Component.Component ()
import Shared.Model.Component.Component
import Shared.Model.Context.RequestContext

entityName = "component"

pageLabel = "components"

findComponents :: RequestContextC s sc m => m [Component]
findComponents = createFindEntitiesFn entityName

insertComponent :: RequestContextC s sc m => Component -> m Int64
insertComponent = createInsertFn entityName

deleteComponents :: RequestContextC s sc m => m Int64
deleteComponents = createDeleteEntitiesFn entityName
