module Shared.Database.DAO.Registry.RegistryTemplateDAO where

import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Registry.RegistryTemplate ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Registry.RegistryTemplate

entityName = "registry_document_template"

findRegistryTemplates :: WizardRequestContextC s m => m [RegistryTemplate]
findRegistryTemplates = createFindEntitiesFn entityName

insertRegistryTemplate :: WizardRequestContextC s m => RegistryTemplate -> m Int64
insertRegistryTemplate = createInsertFn entityName

deleteRegistryTemplates :: WizardRequestContextC s m => m Int64
deleteRegistryTemplates = createDeleteEntitiesFn entityName
