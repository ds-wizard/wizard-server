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

findDocumentTemplatesPage :: WizardRequestContextC s m => Maybe String -> Maybe String -> Maybe String -> Maybe Bool -> Maybe Bool -> Pageable -> [Sort] -> m (Page DocumentTemplateList)
findDocumentTemplatesPage mOrganizationId mTemplateId mQuery mOutdated mNonEditable pageable sort =
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
    -- 2. Get total count
    count <- countDocumentTemplatesPage mQuery mOrganizationId mTemplateId mOutdated outdatedCondition nonEditableCondition
    -- 3. Get entities
    let sql =
          fromString $
            f'
              "SELECT \
              \   document_template.uuid, \
              \   document_template.name, \
              \   document_template.organization_id, \
              \   document_template.template_id, \
              \   document_template.version, \
              \   document_template.phase, \
              \   document_template.metamodel_version, \
              \   document_template.description, \
              \   document_template.allowed_packages, \
              \   document_template.non_editable, \
              \   document_template.language, \
              \   document_template.pot_file_ready, \
              \   registry_document_template.remote_version, \
              \   registry_organization.name as org_name, \
              \   registry_organization.logo as org_logo, \
              \   document_template.created_at \
              \FROM document_template \
              \LEFT JOIN registry_document_template ON document_template.organization_id = registry_document_template.organization_id AND document_template.template_id = registry_document_template.template_id \
              \LEFT JOIN registry_organization ON document_template.organization_id = registry_organization.organization_id \
              \WHERE tenant_uuid = ? AND concat(document_template.organization_id, ':', document_template.template_id, ':', document_template.version) IN ( \
              \    SELECT CONCAT(organization_id, ':', template_id, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
              \                                                          (max(string_to_array(version, '.')::int[]))[2] || '.' || \
              \                                                          (max(string_to_array(version, '.')::int[]))[3]) \
              \    FROM document_template \
              \    WHERE (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') AND tenant_uuid = ? AND (name ~* ? OR organization_id ~* ? OR template_id ~* ? OR version ~* ?) %s \
              \    GROUP BY organization_id, template_id \
              \) \
              \%s \
              \%s \
              \%s \
              \OFFSET %s \
              \LIMIT %s"
              [ mapToDBCoordinatesSql entityName "template_id" mOrganizationId mTemplateId
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
            : toField (regexM mQuery)
            : fmap toField (mapToDBCoordinatesParams mOrganizationId mTemplateId)
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
            \   document_template.organization_id, \
            \   document_template.template_id, \
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
            \WHERE tenant_uuid = ? AND concat(organization_id, ':', template_id, ':', version) IN ( \
            \    SELECT CONCAT(organization_id, ':', template_id, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
            \                                                          (max(string_to_array(version, '.')::int[]))[2] || '.' || \
            \                                                          (max(string_to_array(version, '.')::int[]))[3]) \
            \    FROM document_template \
            \    WHERE (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') AND tenant_uuid = ? AND (name ~* ? OR organization_id ~* ? OR template_id ~* ? OR version ~* ?) \
            \    GROUP BY organization_id, template_id \
            \) \
            \%s"
            [nonEditableCondition]
  let params = [U.toString tenantUuid, U.toString tenantUuid, regexM mQuery, regexM mQuery, regexM mQuery, regexM mQuery]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

countDocumentTemplatesPage :: WizardRequestContextC s m => Maybe String -> Maybe String -> Maybe String -> Maybe Bool -> String -> String -> m Int
countDocumentTemplatesPage mQuery mOrganizationId mTemplateId mOutdated outdatedCondition nonEditableCondition = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f'
            "SELECT count(*) \
            \FROM document_template \
            \LEFT JOIN registry_document_template ON document_template.organization_id = registry_document_template.organization_id AND document_template.template_id = registry_document_template.template_id \
            \WHERE tenant_uuid = ? AND (name ~* ? OR document_template.organization_id ~* ? OR document_template.template_id ~* ? OR document_template.version ~* ?) %s %s %s \
            \  AND CONCAT(document_template.organization_id, ':', document_template.template_id, ':', document_template.version) IN \
            \          (SELECT CONCAT(organization_id, ':', template_id, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
            \                                                                 (max(string_to_array(version, '.')::int[]))[2] || '.' || \
            \                                                                 (max(string_to_array(version, '.')::int[]))[3]) \
            \             FROM document_template \
            \             WHERE (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') AND tenant_uuid = ? AND (name ~* ? OR organization_id ~* ? OR template_id ~* ? OR version ~* ?) \
            \             GROUP BY organization_id, template_id)"
            [ mapToDBCoordinatesSql entityName "template_id" mOrganizationId mTemplateId
            , outdatedCondition
            , nonEditableCondition
            ]
  let params =
        toField tenantUuid
          : toField (regexM mQuery)
          : toField (regexM mQuery)
          : toField (regexM mQuery)
          : toField (regexM mQuery)
          : fmap toField (mapToDBCoordinatesParams mOrganizationId mTemplateId)
          ++ (maybeToList . fmap toField $ mOutdated)
          ++ [toField tenantUuid, toField $ regexM mQuery, toField $ regexM mQuery, toField $ regexM mQuery, toField $ regexM mQuery]
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

findDocumentTemplatesFiltered :: WizardRequestContextC s m => [(String, String)] -> m [DocumentTemplate]
findDocumentTemplatesFiltered queryParams = do
  tenantUuid <- asks (.tenantUuid')
  let queryCondition =
        case queryParams of
          [] -> ""
          _ -> f' "AND %s" [mapToDBQuerySql queryParams]
  let sql =
        fromString $
          f'
            "SELECT * \
            \FROM document_template \
            \WHERE tenant_uuid = ? AND (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') %s"
            [queryCondition]
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
