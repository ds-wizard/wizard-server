module Shared.Database.DAO.Registry.RegistryOrganizationDAO where

import Data.String
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Registry.RegistryOrganization ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Registry.RegistryOrganization
import Shared.Util.String

entityName = "registry_organization"

findRegistryOrganizations :: WizardRequestContextC s m => m [RegistryOrganization]
findRegistryOrganizations = createFindEntitiesFn entityName

insertRegistryOrganization :: WizardRequestContextC s m => RegistryOrganization -> m Int64
insertRegistryOrganization = createInsertFn entityName

deleteRegistryOrganizations :: WizardRequestContextC s m => m Int64
deleteRegistryOrganizations = createDeleteEntitiesFn entityName

deleteRegistryOrganizationsByOrganizationIds :: WizardRequestContextC s m => [String] -> m ()
deleteRegistryOrganizationsByOrganizationIds organizationIds = do
  let sql =
        fromString $
          f' "DELETE FROM %s WHERE organization_id IN (%s)" [entityName, generateQuestionMarks organizationIds]
  let params = organizationIds
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return ()
