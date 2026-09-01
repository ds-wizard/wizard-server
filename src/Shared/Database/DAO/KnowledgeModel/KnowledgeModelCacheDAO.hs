module Shared.Database.DAO.KnowledgeModel.KnowledgeModelCacheDAO where

import Control.Monad.Reader (asks)
import Data.Bifunctor (second)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.KnowledgeModel.Cache.KnowledgeModelCache ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.KnowledgeModelCache

entityName = "knowledge_model_cache"

findKnowledgeModelCacheByUuid' :: WizardRequestContextC s m => U.UUID -> [U.UUID] -> U.UUID -> m (Maybe KnowledgeModelCache)
findKnowledgeModelCacheByUuid' pkgUuid tagUuids tenantUuid = do
  let sql =
        fromString
          "SELECT * \
          \FROM knowledge_model_cache \
          \WHERE package_uuid = ? AND tag_uuids = ? AND tenant_uuid = ?"
  let params = [toField pkgUuid, toField tagUuids, toField tenantUuid]
  let queryParams = [("package_uuid", U.toString pkgUuid), ("tag_uuids", show tagUuids), ("tenant_uuid", U.toString tenantUuid)]
  logQuery sql params
  let action conn = query conn sql params
  runOneEntityDB' entityName action queryParams

insertKnowledgeModelCache :: WizardRequestContextC s m => KnowledgeModelCache -> m Int64
insertKnowledgeModelCache kmCache = do
  let sql = fromString "INSERT INTO knowledge_model_cache VALUES (?, ?, ?, ?, ?) ON CONFLICT DO NOTHING"
  let params = toRow kmCache
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

findMissingKnowledgeModelCacheKeys :: WizardRequestContextC s m => m [(U.UUID, [U.UUID])]
findMissingKnowledgeModelCacheKeys = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "SELECT key.package_uuid, key.tag_uuids \
          \FROM (SELECT knowledge_model_package_uuid AS package_uuid, selected_question_tag_uuids AS tag_uuids \
          \      FROM project \
          \      WHERE tenant_uuid = ? \
          \      UNION \
          \      SELECT uuid, '{}'::uuid[] \
          \      FROM knowledge_model_package \
          \      WHERE tenant_uuid = ?) key \
          \LEFT JOIN knowledge_model_cache ON knowledge_model_cache.package_uuid = key.package_uuid \
          \    AND knowledge_model_cache.tag_uuids = key.tag_uuids::text[] \
          \    AND knowledge_model_cache.tenant_uuid = ? \
          \WHERE knowledge_model_cache.package_uuid IS NULL"
  let params = [toField tenantUuid, toField tenantUuid, toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  keys <- runDB action
  return $ fmap (second fromPGArray) keys
