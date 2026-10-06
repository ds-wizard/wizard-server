module Shared.Database.DAO.DocumentTemplate.WizardDocumentTemplateDAO where

import Control.Monad.Reader (asks, liftIO)
import Data.Maybe (maybeToList)
import Data.String (fromString)
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Database.DAO.WizardCommon hiding (createCountGroupByCoordinateFn, createFindEntitiesGroupByCoordinatePageableQuerySortFn)
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplate ()
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplateList ()
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplateSuggestion ()
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateList
import Shared.Model.DocumentTemplate.DocumentTemplateSuggestion
import Shared.Util.String

entityName = "document_template"

pageLabel = "documentTemplates"

findDocumentTemplatesPage :: WizardRequestContextC s m => Maybe String -> Maybe String -> Maybe Bool -> Maybe Bool -> Pageable -> [Sort] -> m (Page DocumentTemplateList)
findDocumentTemplatesPage mId mQuery mOutdated mNonEditable pageable sort =
  -- 1. Prepare variables
  do
    tenantUuid <- asks (.tenantUuid')
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    let outdatedCondition =
          case mOutdated of
            Just _ -> "AND is_outdated(registry_document_template.remote_version, document_template.version) = ?"
            Nothing -> ""
    let nonEditableCondition =
          case mNonEditable of
            Just nonEditable -> f' "AND non_editable = '%s'" [show nonEditable]
            Nothing -> ""
    workspaceCondition <- tenantOrWorkspaceCondition Nothing "document_template.workspace_uuid"
    -- 2. Get total count
    count <- countDocumentTemplatesPage mQuery mId mOutdated outdatedCondition nonEditableCondition workspaceCondition
    -- 3. Get entities
    let sql =
          fromString $
            f'
              "SELECT \
              \   document_template.uuid, \
              \   document_template.name, \
              \   document_template.id, \
              \   document_template.version, \
              \   document_template.phase, \
              \   document_template.metamodel_version, \
              \   document_template.description, \
              \   document_template.allowed_packages, \
              \   document_template.non_editable, \
              \   document_template.language, \
              \   document_template.pot_file_ready, \
              \   registry_document_template.remote_version, \
              \   document_template.created_at, \
              \   document_template.workspace_uuid \
              \FROM document_template \
              \LEFT JOIN registry_document_template ON document_template.id = registry_document_template.id \
              \WHERE tenant_uuid = ? AND concat(document_template.id, ':', document_template.version, ':', document_template.workspace_uuid) IN ( \
              \    SELECT CONCAT(id, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
              \                                                          (max(string_to_array(version, '.')::int[]))[2] || '.' || \
              \                                                          (max(string_to_array(version, '.')::int[]))[3], ':', document_template.workspace_uuid) \
              \    FROM document_template \
              \    WHERE (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') AND tenant_uuid = ? AND (name ~* ? OR id ~* ? OR version ~* ?) %s %s \
              \    GROUP BY id, document_template.workspace_uuid \
              \) \
              \%s \
              \%s \
              \%s \
              \OFFSET %s \
              \LIMIT %s"
              [ mapToDBIdSql entityName mId
              , workspaceCondition
              , outdatedCondition
              , nonEditableCondition
              , mapSort sort
              , show skip
              , show sizeI
              ]
    let params =
          toField tenantUuid
            : toField tenantUuid
            : toField (regexM mQuery)
            : toField (regexM mQuery)
            : toField (regexM mQuery)
            : fmap toField (mapToDBIdParams mId)
            ++ (maybeToList . fmap toField $ mOutdated)
    logQuery sql params
    let action conn = query conn sql params
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

findDocumentTemplatesSuggestions :: WizardRequestContextC s m => Maybe String -> Maybe Bool -> m [DocumentTemplateSuggestion]
findDocumentTemplatesSuggestions mQuery mNonEditable = do
  tenantUuid <- asks (.tenantUuid')
  workspaceCondition <- tenantOrWorkspaceCondition Nothing "document_template.workspace_uuid"
  let nonEditableCondition =
        case mNonEditable of
          Just nonEditable -> f' "AND non_editable = '%s'" [show nonEditable]
          Nothing -> ""
  let sql =
        fromString $
          f'
            "SELECT \
            \   document_template.uuid, \
            \   document_template.name, \
            \   document_template.id, \
            \   document_template.version, \
            \   document_template.phase, \
            \   document_template.metamodel_version, \
            \   document_template.description, \
            \   document_template.language, \
            \   document_template.allowed_packages, \
            \   ( \
            \    SELECT coalesce(jsonb_agg(jsonb_build_object('uuid', uuid, 'name', name, 'icon', icon)), '[]'::jsonb) \
            \    FROM (SELECT * \
            \          FROM document_template_format dt_format \
            \          WHERE dt_format.tenant_uuid = document_template.tenant_uuid \
            \            AND dt_format.document_template_uuid = document_template.uuid \
            \          ORDER BY dt_format.name) nested \
            \   ) AS document_template_formats, \
            \   ( \
            \    SELECT coalesce(jsonb_agg(jsonb_build_object('uuid', uuid, 'name', name, 'code', code)), '[]'::jsonb) \
            \    FROM (SELECT * \
            \          FROM document_template_locale dt_locale \
            \          WHERE dt_locale.tenant_uuid = document_template.tenant_uuid \
            \            AND dt_locale.document_template_uuid = document_template.uuid \
            \          ORDER BY dt_locale.name) nested \
            \   ) AS document_template_locales \
            \FROM document_template \
            \WHERE tenant_uuid = ? AND concat(id, ':', version, ':', workspace_uuid) IN ( \
            \    SELECT CONCAT(id, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
            \                                                          (max(string_to_array(version, '.')::int[]))[2] || '.' || \
            \                                                          (max(string_to_array(version, '.')::int[]))[3], ':', workspace_uuid) \
            \    FROM document_template \
            \    WHERE (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') AND tenant_uuid = ? AND (name ~* ? OR id ~* ? OR version ~* ?) %s \
            \    GROUP BY id, workspace_uuid \
            \) \
            \%s"
            [workspaceCondition, nonEditableCondition]
  let params = [U.toString tenantUuid, U.toString tenantUuid, regexM mQuery, regexM mQuery, regexM mQuery]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

countDocumentTemplatesPage :: WizardRequestContextC s m => Maybe String -> Maybe String -> Maybe Bool -> String -> String -> String -> m Int
countDocumentTemplatesPage mQuery mId mOutdated outdatedCondition nonEditableCondition workspaceCondition = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f'
            "SELECT count(*) \
            \FROM document_template \
            \LEFT JOIN registry_document_template ON document_template.id = registry_document_template.id \
            \WHERE tenant_uuid = ? AND (name ~* ? OR document_template.id ~* ? OR document_template.version ~* ?) %s %s %s \
            \  AND CONCAT(document_template.id, ':', document_template.version, ':', document_template.workspace_uuid) IN \
            \          (SELECT CONCAT(id, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
            \                                                                 (max(string_to_array(version, '.')::int[]))[2] || '.' || \
            \                                                                 (max(string_to_array(version, '.')::int[]))[3], ':', document_template.workspace_uuid) \
            \             FROM document_template \
            \             WHERE (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') AND tenant_uuid = ? AND (name ~* ? OR id ~* ? OR version ~* ?) %s \
            \             GROUP BY id, document_template.workspace_uuid)"
            [ mapToDBIdSql entityName mId
            , outdatedCondition
            , nonEditableCondition
            , workspaceCondition
            ]
  let params =
        toField tenantUuid
          : toField (regexM mQuery)
          : toField (regexM mQuery)
          : toField (regexM mQuery)
          : fmap toField (mapToDBIdParams mId)
          ++ (maybeToList . fmap toField $ mOutdated)
          ++ [toField tenantUuid, toField $ regexM mQuery, toField $ regexM mQuery, toField $ regexM mQuery]
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

findDocumentTemplatesFiltered :: WizardRequestContextC s m => [(String, String)] -> m [DocumentTemplate]
findDocumentTemplatesFiltered queryParams = do
  tenantUuid <- asks (.tenantUuid')
  workspaceCondition <- tenantOrWorkspaceCondition Nothing "workspace_uuid"
  let queryCondition =
        case queryParams of
          [] -> ""
          _ -> f' "AND %s" [mapToDBQuerySql queryParams]
  let sql =
        fromString $
          f'
            "SELECT * \
            \FROM document_template \
            \WHERE tenant_uuid = ? AND (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') %s %s"
            [workspaceCondition, queryCondition]
  let params = U.toString tenantUuid : fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runDB action

touchDocumentTemplateByUuid :: WizardRequestContextC s m => U.UUID -> m Int64
touchDocumentTemplateByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  now <- liftIO getCurrentTime
  let sql = fromString "UPDATE document_template SET updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField now, toField tenantUuid, toField uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
