module Shared.Database.DAO.Registry.RegistryMailTemplateLocaleDAO where

import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Registry.RegistryMailTemplateLocale ()
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Registry.RegistryMailTemplateLocale

entityName = "registry_mail_template_locale"

findRegistryMailTemplateLocales :: WizardRequestContextC s m => m [RegistryMailTemplateLocale]
findRegistryMailTemplateLocales = createFindEntitiesSortedFn entityName [Sort "name" Ascending]

findRegistryMailTemplateLocaleByCode :: WizardRequestContextC s m => String -> m RegistryMailTemplateLocale
findRegistryMailTemplateLocaleByCode code = createFindEntityByFn entityName [("code", code)]

insertRegistryMailTemplateLocale :: WizardRequestContextC s m => RegistryMailTemplateLocale -> m Int64
insertRegistryMailTemplateLocale = createInsertFn entityName

deleteRegistryMailTemplateLocales :: WizardRequestContextC s m => m Int64
deleteRegistryMailTemplateLocales = createDeleteEntitiesFn entityName
