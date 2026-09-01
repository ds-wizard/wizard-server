module Shared.Database.DAO.KnowledgeModel.KnowledgeModelLocaleDAO where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.KnowledgeModel.Locale.KnowledgeModelLocale ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocale

entityName = "knowledge_model_locale"

findKnowledgeModelLocalesByPackageUuid :: WizardRequestContextC s m => U.UUID -> m [KnowledgeModelLocale]
findKnowledgeModelLocalesByPackageUuid pkgUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("knowledge_model_package_uuid", U.toString pkgUuid)]

findKnowledgeModelLocaleByUuid :: WizardRequestContextC s m => U.UUID -> m KnowledgeModelLocale
findKnowledgeModelLocaleByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findKnowledgeModelLocaleByPackageUuidAndUuid :: WizardRequestContextC s m => U.UUID -> U.UUID -> m KnowledgeModelLocale
findKnowledgeModelLocaleByPackageUuidAndUuid pkgUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("knowledge_model_package_uuid", U.toString pkgUuid), ("uuid", U.toString uuid)]

findKnowledgeModelLocaleByPackageUuidAndCode' :: WizardRequestContextC s m => U.UUID -> String -> m (Maybe KnowledgeModelLocale)
findKnowledgeModelLocaleByPackageUuidAndCode' pkgUuid code = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("knowledge_model_package_uuid", U.toString pkgUuid), ("code", code)]

insertKnowledgeModelLocale :: WizardRequestContextC s m => KnowledgeModelLocale -> m Int64
insertKnowledgeModelLocale = createInsertFn entityName

deleteKnowledgeModelLocales :: WizardRequestContextC s m => m Int64
deleteKnowledgeModelLocales = createDeleteEntitiesFn entityName

deleteKnowledgeModelLocaleByUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteKnowledgeModelLocaleByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
