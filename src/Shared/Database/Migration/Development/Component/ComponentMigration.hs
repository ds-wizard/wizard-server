module Shared.Database.Migration.Development.Component.ComponentMigration where

import Shared.Constant.Component
import Shared.Database.DAO.Component.ComponentDAO
import Shared.Database.Migration.Development.Component.Data.Components
import Shared.Model.Context.RequestContext
import Shared.Util.Logger

runMigration :: RequestContextC s sc m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Component/Component) started"
  deleteComponents
  insertComponent mailComponent
  logInfo _CMP_MIGRATION "(Component/Component) ended"
