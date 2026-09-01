module Shared.Database.DAO.DocumentTemplate.DocumentTemplateLocaleDAO where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.DocumentTemplate.Locale.DocumentTemplateLocale ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocale

entityName = "document_template_locale"

findDocumentTemplateLocalesByDocumentTemplateUuid :: WizardRequestContextC s m => U.UUID -> m [DocumentTemplateLocale]
findDocumentTemplateLocalesByDocumentTemplateUuid dtUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString dtUuid)]

findDocumentTemplateLocaleByDocumentTemplateUuidAndUuid :: WizardRequestContextC s m => U.UUID -> U.UUID -> m DocumentTemplateLocale
findDocumentTemplateLocaleByDocumentTemplateUuidAndUuid dtUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString dtUuid), ("uuid", U.toString uuid)]

findDocumentTemplateLocaleByDocumentTemplateUuidAndCode' :: WizardRequestContextC s m => U.UUID -> String -> m (Maybe DocumentTemplateLocale)
findDocumentTemplateLocaleByDocumentTemplateUuidAndCode' dtUuid code = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString dtUuid), ("code", code)]

insertDocumentTemplateLocale :: WizardRequestContextC s m => DocumentTemplateLocale -> m Int64
insertDocumentTemplateLocale = createInsertFn entityName

deleteDocumentTemplateLocales :: WizardRequestContextC s m => m Int64
deleteDocumentTemplateLocales = createDeleteEntitiesFn entityName

deleteDocumentTemplateLocaleByUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteDocumentTemplateLocaleByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
