module Shared.Database.DAO.Library.LibraryDependentDAO where

import Control.Monad.Reader (asks)
import qualified Data.List as L
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple

import Shared.Database.DAO.Project.ProjectDAO (projectVisibleCondition)
import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Library.LibraryDependent ()
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Library.LibraryDependents
import Shared.Util.Logger
import Shared.Util.String (f'', trim)

findPackageDependents :: WizardRequestContextC s m => [U.UUID] -> m [LibraryDependent]
findPackageDependents pkgUuids = do
  tenantUuid <- asks (.tenantUuid')
  reachAccess <- workspaceAccess Nothing
  editorAccess <- workspaceAccess (Just _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION)
  projectVisible <- projectVisibleCondition
  runDependentsQuery $
    f''
      "WITH RECURSIVE package_tree AS (SELECT uuid \
      \                                FROM knowledge_model_package \
      \                                WHERE tenant_uuid = '${tenantUuid}' AND uuid IN (${pkgUuids}) \
      \                                UNION \
      \                                SELECT p.uuid \
      \                                FROM knowledge_model_package p \
      \                                JOIN package_tree pt ON p.previous_package_uuid = pt.uuid \
      \                                WHERE p.tenant_uuid = '${tenantUuid}') \
      \SELECT 'PackageLibraryDependentEntity', p.uuid, p.name, p.id, p.version, p.workspace_uuid, p.workspace_uuid IS NULL OR ${packageVisible} \
      \FROM knowledge_model_package p \
      \WHERE p.uuid IN (SELECT uuid FROM package_tree) AND p.uuid NOT IN (${pkgUuids}) \
      \UNION ALL \
      \SELECT 'EditorLibraryDependentEntity', e.uuid, e.name, NULL, NULL, e.workspace_uuid, ${editorVisible} \
      \FROM knowledge_model_editor e \
      \WHERE e.tenant_uuid = '${tenantUuid}' AND e.previous_package_uuid IN (SELECT uuid FROM package_tree) \
      \UNION ALL \
      \SELECT 'ProjectLibraryDependentEntity', project.uuid, project.name, NULL, NULL, project.workspace_uuid, ${projectVisible} \
      \FROM project \
      \WHERE project.tenant_uuid = '${tenantUuid}' AND project.knowledge_model_package_uuid IN (SELECT uuid FROM package_tree) \
      \ORDER BY 1, 3, 2"
      [ ("tenantUuid", U.toString tenantUuid)
      , ("pkgUuids", uuidList pkgUuids)
      , ("packageVisible", workspaceAccessSql "p.workspace_uuid" reachAccess)
      , ("editorVisible", workspaceAccessSql "e.workspace_uuid" editorAccess)
      , ("projectVisible", projectVisible)
      ]

findDocumentTemplateDependents :: WizardRequestContextC s m => [U.UUID] -> m [LibraryDependent]
findDocumentTemplateDependents dtUuids = do
  tenantUuid <- asks (.tenantUuid')
  projectVisible <- projectVisibleCondition
  runDependentsQuery $
    f''
      "SELECT 'ProjectLibraryDependentEntity', project.uuid, project.name, NULL, NULL, project.workspace_uuid, ${projectVisible} \
      \FROM project \
      \WHERE project.tenant_uuid = '${tenantUuid}' AND project.document_template_uuid IN (${dtUuids}) \
      \UNION ALL \
      \SELECT 'DocumentLibraryDependentEntity', d.uuid, d.name, NULL, NULL, d.workspace_uuid, COALESCE(${projectVisible}, FALSE) \
      \FROM document d \
      \LEFT JOIN project ON project.uuid = d.project_uuid \
      \WHERE d.tenant_uuid = '${tenantUuid}' AND d.document_template_uuid IN (${dtUuids}) AND d.durability = 'PersistentDocumentDurability' \
      \ORDER BY 1, 3, 2"
      [ ("tenantUuid", U.toString tenantUuid)
      , ("dtUuids", uuidList dtUuids)
      , ("projectVisible", projectVisible)
      ]

runDependentsQuery :: WizardRequestContextC s m => String -> m [LibraryDependent]
runDependentsQuery sql = do
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  runDB action

uuidList :: [U.UUID] -> String
uuidList = L.intercalate ", " . fmap (\u -> "'" ++ U.toString u ++ "'")
