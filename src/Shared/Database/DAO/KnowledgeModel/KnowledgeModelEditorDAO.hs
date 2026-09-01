module Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.KnowledgeModel.Editor.KnowledgeModelEditor ()
import Shared.Database.Mapping.KnowledgeModel.Editor.KnowledgeModelEditorList ()
import Shared.Database.Mapping.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion ()
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion
import Shared.Util.Logger
import Shared.Util.String (trim)

entityName = "knowledge_model_editor"

pageLabel = "knowledgeModelEditors"

findKnowledgeModelEditors :: WizardRequestContextC s m => m [KnowledgeModelEditor]
findKnowledgeModelEditors = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findKnowledgeModelEditorsByPreviousPackageUuid :: WizardRequestContextC s m => U.UUID -> m [KnowledgeModelEditor]
findKnowledgeModelEditorsByPreviousPackageUuid previousPackageUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("previous_package_uuid", U.toString previousPackageUuid)]

findKnowledgeModelEditorsPage :: WizardRequestContextC s m => Maybe String -> Pageable -> [Sort] -> m (Page KnowledgeModelEditorList)
findKnowledgeModelEditorsPage mQuery pageable sort =
  -- 1. Prepare variables
  do
    tenantUuid <- asks (.tenantUuid')
    let condition = "WHERE (name ~* ? OR km_id ~* ?) AND tenant_uuid = ?"
    let conditionParams = [regexM mQuery, regexM mQuery, U.toString tenantUuid]
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    count <- createCountByFn entityName condition conditionParams
    -- 3. Get entities
    let sql =
          fromString $
            f'
              "SELECT knowledge_model_editor.uuid, \
              \       knowledge_model_editor.name, \
              \       knowledge_model_editor.km_id, \
              \       knowledge_model_editor.version, \
              \       get_knowledge_model_editor_state(knowledge_model_editor, knowledge_model_migration, get_knowledge_model_editor_fork_of_package_id(config_organization, previous_pkg, knowledge_model_editor), '%s') as state, \
              \       knowledge_model_editor.previous_package_uuid, \
              \       fork_of_package.uuid, \
              \       fork_of_package.name, \
              \       fork_of_package.organization_id, \
              \       fork_of_package.km_id, \
              \       fork_of_package.version, \
              \       fork_of_package.description, \
              \       knowledge_model_editor.created_by, \
              \       knowledge_model_editor.created_at, \
              \       GREATEST(knowledge_model_editor.updated_at, (SELECT MAX(created_at) FROM knowledge_model_editor_event WHERE knowledge_model_editor_event.editor_uuid = knowledge_model_editor.uuid AND knowledge_model_editor_event.tenant_uuid = knowledge_model_editor.tenant_uuid)) AS updated_at  \
              \FROM knowledge_model_editor \
              \JOIN config_organization ON knowledge_model_editor.tenant_uuid = config_organization.tenant_uuid \
              \LEFT JOIN knowledge_model_migration ON knowledge_model_editor.uuid = knowledge_model_migration.editor_uuid \
              \LEFT JOIN knowledge_model_package previous_pkg ON knowledge_model_editor.previous_package_uuid = previous_pkg.uuid and knowledge_model_editor.tenant_uuid = previous_pkg.tenant_uuid \
              \LEFT JOIN knowledge_model_package fork_of_package ON get_knowledge_model_editor_fork_of_package_id(config_organization, previous_pkg, knowledge_model_editor) = concat(fork_of_package.organization_id, ':', fork_of_package.km_id, ':', fork_of_package.version) AND knowledge_model_editor.tenant_uuid = fork_of_package.tenant_uuid \
              \WHERE (knowledge_model_editor.name ~* ? OR knowledge_model_editor.km_id ~* ?) AND knowledge_model_editor.tenant_uuid = ? \
              \%s OFFSET %s LIMIT %s"
              [U.toString tenantUuid, mapSort sort, show skip, show sizeI]
    logQuery sql conditionParams
    let action conn = query conn sql conditionParams
    entities <- runDB action
    -- 4. Constructor response
    let metadata =
          PageMetadata
            { size = sizeI
            , totalElements = count
            , totalPages = computeTotalPage count sizeI
            , number = pageI
            }
    return $ Page pageLabel metadata entities

findEditorsByUnsupportedMetamodelVersion :: WizardRequestContextC s m => Int -> m [KnowledgeModelEditor]
findEditorsByUnsupportedMetamodelVersion metamodelVersion = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "SELECT * FROM knowledge_model_editor WHERE metamodel_version != ? AND tenant_uuid = ?"
  let params = [toField metamodelVersion, toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findEditorsForSquashing :: WizardRequestContextC s m => m [U.UUID]
findEditorsForSquashing = do
  let sql = "SELECT uuid FROM knowledge_model_editor WHERE squashed = false"
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  entities <- runDB action
  return . concat $ entities

findKnowledgeModelEditorByUuidForSquashingLocked :: WizardRequestContextC s m => U.UUID -> m KnowledgeModelEditor
findKnowledgeModelEditorByUuidForSquashingLocked editorUuid = createFindEntityWithFieldsByFn "*" True entityName [("uuid", U.toString editorUuid)]

findKnowledgeModelEditorSuggestionsPage :: WizardRequestContextC s m => Maybe String -> Pageable -> [Sort] -> m (Page KnowledgeModelEditorSuggestion)
findKnowledgeModelEditorSuggestionsPage mQuery pageable sort = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "uuid, name"
    "WHERE name ~* ? AND tenant_uuid = ?"
    [regexM mQuery, U.toString tenantUuid]

findKnowledgeModelEditorByUuid :: WizardRequestContextC s m => U.UUID -> m KnowledgeModelEditor
findKnowledgeModelEditorByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findKnowledgeModelEditorSuggestionByUuid' :: WizardRequestContextC s m => U.UUID -> m (Maybe KnowledgeModelEditorSuggestion)
findKnowledgeModelEditorSuggestionByUuid' uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityWithFieldsByFn' "uuid, name" entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

countKnowledgeModelEditors :: WizardRequestContextC s m => m Int
countKnowledgeModelEditors = do
  tenantUuid <- asks (.tenantUuid')
  countKnowledgeModelEditorsWithTenant tenantUuid

countKnowledgeModelEditorsWithTenant :: WizardRequestContextC s m => U.UUID -> m Int
countKnowledgeModelEditorsWithTenant tenantUuid = createCountByFn entityName tenantCondition [U.toString tenantUuid]

insertKnowledgeModelEditor :: WizardRequestContextC s m => KnowledgeModelEditor -> m Int64
insertKnowledgeModelEditor = createInsertFn entityName

updateKnowledgeModelEditorByUuid :: WizardRequestContextC s m => KnowledgeModelEditor -> m Int64
updateKnowledgeModelEditorByUuid kmEditor = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "UPDATE knowledge_model_editor SET uuid = ?, name = ?, km_id = ?, previous_package_uuid = ?, created_by = ?, created_at = ?, updated_at = ?, tenant_uuid = ?, version = ?, description = ?, readme = ?, license = ?, metamodel_version = ?, squashed = ?, language = ? WHERE tenant_uuid = ? AND uuid = ?;"
  let params = toRow kmEditor ++ [toField tenantUuid, toField . U.toText $ kmEditor.uuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

updateKnowledgeModelEditorMetamodelVersion :: WizardRequestContextC s m => U.UUID -> Int -> m Int64
updateKnowledgeModelEditorMetamodelVersion uuid metamodelVersion = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "UPDATE knowledge_model_editor SET metamodel_version = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField metamodelVersion, toField tenantUuid, toField uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteKnowledgeModelEditors :: WizardRequestContextC s m => m Int64
deleteKnowledgeModelEditors = createDeleteEntitiesFn entityName

deleteKnowledgeModelEditorByUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteKnowledgeModelEditorByUuid uuid = createDeleteEntityByFn entityName [("uuid", U.toString uuid)]
