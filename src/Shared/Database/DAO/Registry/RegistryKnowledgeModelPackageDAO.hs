module Shared.Database.DAO.Registry.RegistryKnowledgeModelPackageDAO where

import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Registry.RegistryPackage ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Registry.RegistryPackage

entityName = "registry_knowledge_model_package"

findRegistryPackages :: WizardRequestContextC s m => m [RegistryPackage]
findRegistryPackages = createFindEntitiesFn entityName

insertRegistryPackage :: WizardRequestContextC s m => RegistryPackage -> m Int64
insertRegistryPackage = createInsertFn entityName

deleteRegistryPackages :: WizardRequestContextC s m => m Int64
deleteRegistryPackages = createDeleteEntitiesFn entityName
