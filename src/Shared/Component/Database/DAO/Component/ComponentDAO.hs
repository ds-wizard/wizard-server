module Shared.Component.Database.DAO.Component.ComponentDAO where

import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Database.Mapping.Component.Component ()
import Shared.Common.Model.Context.AppContext
import Shared.Component.Model.Component.Component

entityName = "component"

pageLabel = "components"

findComponents :: AppContextC s sc m => m [Component]
findComponents = do
  table <- tableName entityName
  createFindEntitiesFn table

insertComponent :: AppContextC s sc m => Component -> m Int64
insertComponent component = do
  table <- tableName entityName
  createInsertFn table component

deleteComponents :: AppContextC s sc m => m Int64
deleteComponents = do
  table <- tableName entityName
  createDeleteEntitiesFn table
