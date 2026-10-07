module Shared.Database.DAO.Registry.RegistryLocaleDAO where

import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Registry.RegistryLocale ()
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Registry.RegistryLocale

entityName = "registry_locale"

findRegistryLocales :: WizardRequestContextC s m => m [RegistryLocale]
findRegistryLocales = createFindEntitiesSortedFn entityName [Sort "name" Ascending]

findRegistryLocaleByCode :: WizardRequestContextC s m => String -> m RegistryLocale
findRegistryLocaleByCode code = createFindEntityByFn entityName [("code", code)]

insertRegistryLocale :: WizardRequestContextC s m => RegistryLocale -> m Int64
insertRegistryLocale = createInsertFn entityName

deleteRegistryLocales :: WizardRequestContextC s m => m Int64
deleteRegistryLocales = createDeleteEntitiesFn entityName
