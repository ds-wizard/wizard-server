module Wizard.Database.DAO.Project.ProjectDAO where

import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.String (fromString)
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Util.Logger
import Shared.Common.Util.String (f'', trim)
import Shared.Coordinate.Model.Coordinate.Coordinate
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Project.ProjectPermDAO (
  deleteProjectPermsFiltered,
  findProjectPermsFiltered,
  insertProjectPerm,
 )
import Wizard.Database.Mapping.Project.Project ()
import Wizard.Database.Mapping.Project.ProjectDetail ()
import Wizard.Database.Mapping.Project.ProjectDetailPreview ()
import Wizard.Database.Mapping.Project.ProjectDetailQuestionnaire ()
import Wizard.Database.Mapping.Project.ProjectDetailSettings ()
import Wizard.Database.Mapping.Project.ProjectList ()
import Wizard.Database.Mapping.Project.ProjectSimple ()
import Wizard.Database.Mapping.Project.ProjectSimpleWithPerm ()
import Wizard.Database.Mapping.Project.ProjectSuggestion ()
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Project.Detail.ProjectDetail
import Wizard.Model.Project.Detail.ProjectDetailPreview
import Wizard.Model.Project.Detail.ProjectDetailQuestionnaire
import Wizard.Model.Project.Detail.ProjectDetailSettings
import Wizard.Model.Project.Project
import Wizard.Model.Project.ProjectList
import Wizard.Model.Project.ProjectSimpleWithPerm
import Wizard.Model.Project.ProjectSuggestion

entityName = "w_project"

pageLabel = "projects"

findProjects :: AppContextM [Project]
findProjects = do
  tenantUuid <- asks currentTenantUuid
  currentUser <- getCurrentUser
  hasPermission <- hasPermission _PROJECTS_VIEW_ROLE_PERMISSION
  if hasPermission
    then createFindEntitiesBySortedFn entityName [tenantQueryUuid tenantUuid] [Sort "name" Ascending] >>= traverse enhance
    else do
      let sql = f' (projectSelectSql (U.toString tenantUuid) (U.toString currentUser.uuid) "['VIEW']") [""] ++ " ORDER BY w_project.name ASC"
      logInfoI _CMP_DATABASE sql
      let action conn = query_ conn (fromString sql)
      entities <- runDB action
      traverse enhance entities

findProjectsForCurrentUserPage :: Maybe String -> Maybe Bool -> Maybe [String] -> Maybe String -> Maybe [U.UUID] -> Maybe String -> Maybe [U.UUID] -> Maybe String -> Maybe [Coordinate] -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page ProjectList)
findProjectsForCurrentUserPage mQuery mIsTemplate mProjectTags mProjectTagsOp mUserUuids mUserUuidsOp mUserGroupUuids mUserGroupUuidsOp mKnowledgeModelPackageCoordinates mKnowledgeModelPackageCoordinatesOp pageable sort =
  -- 1. Prepare variables
  do
    tenantUuid <- asks currentTenantUuid
    currentUser <- getCurrentUser
    let (nameCondition, nameRegex) =
          case mQuery of
            Just query -> (" AND w_project.name ~* ?", [regex query])
            Nothing -> ("", [])
    let isTemplateCondition =
          case mIsTemplate of
            Nothing -> ""
            Just True -> " AND w_project.is_template = true"
            Just False -> " AND w_project.is_template = false"
    let (projectTagsCondition, projectTagsParam) =
          case mProjectTags of
            Nothing -> ("", [])
            Just [] -> ("", [])
            Just projectTags ->
              let mapFn _ = " w_project.project_tags @> ARRAY [?]"
               in if isAndOperator mProjectTagsOp
                    then (" AND (" ++ L.intercalate " AND " (fmap mapFn projectTags) ++ ")", projectTags)
                    else (" AND (" ++ L.intercalate " OR " (fmap mapFn projectTags) ++ ")", projectTags)
    let userUuidsJoin =
          case mUserUuids of
            Nothing -> ""
            Just [] -> ""
            Just _ -> "LEFT JOIN w_project_perm_user ON w_project.uuid = w_project_perm_user.project_uuid "
    let (userUuidsCondition, userUuidsParam) =
          case mUserUuids of
            Nothing -> ("", [])
            Just [] -> ("", [])
            Just userUuids ->
              if isAndOperator mUserUuidsOp
                then
                  ( f'
                      " AND %s = ( \
                      \SELECT COUNT(DISTINCT user_uuid) \
                      \FROM w_project_perm_user \
                      \WHERE project_uuid = w_project.uuid AND user_uuid in (%s)) "
                      [show . length $ userUuids, generateQuestionMarks userUuids]
                  , fmap U.toString userUuids
                  )
                else
                  let mapFn _ = " w_project_perm_user.user_uuid = ? "
                   in (" AND (" ++ L.intercalate " OR " (fmap mapFn userUuids) ++ ")", fmap U.toString userUuids)
    let userGroupUuidsJoin =
          case mUserGroupUuids of
            Nothing -> ""
            Just [] -> ""
            Just _ -> "LEFT JOIN w_project_perm_group ON w_project.uuid = w_project_perm_group.project_uuid "
    let (userGroupUuidsCondition, userGroupUuidsParam) =
          case mUserGroupUuids of
            Nothing -> ("", [])
            Just [] -> ("", [])
            Just userGroupUuids ->
              if isAndOperator mUserGroupUuidsOp
                then
                  ( f'
                      " AND %s = ( \
                      \SELECT COUNT(DISTINCT user_group_uuid) \
                      \FROM w_project_perm_group \
                      \WHERE project_uuid = w_project.uuid AND user_group_uuid in (%s)) "
                      [show . length $ userGroupUuids, generateQuestionMarks userGroupUuids]
                  , fmap U.toString userGroupUuids
                  )
                else
                  let mapFn _ = " w_project_perm_group.user_group_uuid = ? "
                   in (" AND (" ++ L.intercalate " OR " (fmap mapFn userGroupUuids) ++ ")", fmap U.toString userGroupUuids)
    let (knowledgeModelPackageJoin, knowledgeModelPackageCondition, knowledgeModelPackageIdsParam) =
          case mKnowledgeModelPackageCoordinates of
            Nothing -> ("", "", [])
            Just [] -> ("", "", [])
            Just kmpCoordinates ->
              let operator = if isAndOperator mKnowledgeModelPackageCoordinatesOp then " AND " else " OR "
               in ( "LEFT JOIN w_knowledge_model_package ON w_project.knowledge_model_package_uuid = w_knowledge_model_package.uuid AND w_knowledge_model_package.tenant_uuid = '${tenantUuid}'"
                  , f' " AND (%s)" [L.intercalate operator . fmap (\c -> if c.version == "all" then " (w_knowledge_model_package.organization_id = ? AND w_knowledge_model_package.km_id = ?)" else " (w_knowledge_model_package.organization_id = ? AND w_knowledge_model_package.km_id = ? AND w_knowledge_model_package.version = ?)") $ kmpCoordinates]
                  , concatMap (\c -> if c.version == "all" then [c.organizationId, c.entityId] else [c.organizationId, c.entityId, c.version]) kmpCoordinates
                  )
    hasPermission <- hasPermission _PROJECTS_VIEW_ROLE_PERMISSION
    let (aclJoins, aclCondition) =
          if hasPermission
            then (userUuidsJoin ++ userGroupUuidsJoin, "")
            else
              ( f''
                  "LEFT JOIN w_project_perm_user ON w_project.uuid = w_project_perm_user.project_uuid AND w_project_perm_user.tenant_uuid = '${tenantUuid}' \
                  \LEFT JOIN w_project_perm_group ON w_project.uuid = w_project_perm_group.project_uuid AND w_project_perm_group.tenant_uuid = '${tenantUuid}' \
                  \LEFT JOIN w_user_group_membership ugm ON ugm.user_group_uuid = w_project_perm_group.user_group_uuid AND ugm.user_uuid = '${currentUserUuid}' AND ugm.tenant_uuid = '${tenantUuid}'"
                  [ ("currentUserUuid", U.toString currentUser.uuid)
                  , ("tenantUuid", U.toString tenantUuid)
                  ]
              , f'
                  "AND (visibility = 'VisibleEditProjectVisibility' \
                  \  OR visibility = 'VisibleCommentProjectVisibility' \
                  \  OR visibility = 'VisibleViewProjectVisibility' \
                  \  OR (visibility = 'PrivateProjectVisibility' AND w_project_perm_user.user_uuid = '%s' AND w_project_perm_user.perms @> ARRAY %s) \
                  \  OR (visibility = 'PrivateProjectVisibility' AND w_project_perm_group.user_group_uuid = ugm.user_group_uuid AND w_project_perm_group.perms @> ARRAY %s) \
                  \)"
                  [U.toString currentUser.uuid, "['VIEW']", "['VIEW']"]
              )
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    let countSql =
          fromString $
            f''
              "SELECT COUNT(DISTINCT w_project.uuid) \
              \FROM w_project \
              \${knowledgeModelPackageJoin} \
              \${aclJoins} \
              \WHERE w_project.tenant_uuid = '${tenantUuid}' ${aclCondition} ${nameCondition} ${isTemplateCondition} ${projectTagsCondition} ${userUuidsCondition} ${userGroupUuidsCondition} ${knowledgeModelPackageCondition}"
              [ ("knowledgeModelPackageJoin", knowledgeModelPackageJoin)
              , ("aclJoins", aclJoins)
              , ("tenantUuid", U.toString tenantUuid)
              , ("aclCondition", aclCondition)
              , ("nameCondition", nameCondition)
              , ("isTemplateCondition", isTemplateCondition)
              , ("projectTagsCondition", projectTagsCondition)
              , ("userUuidsCondition", userUuidsCondition)
              , ("userGroupUuidsCondition", userGroupUuidsCondition)
              , ("knowledgeModelPackageCondition", knowledgeModelPackageCondition)
              ]
    let params = nameRegex ++ projectTagsParam ++ userUuidsParam ++ userGroupUuidsParam ++ knowledgeModelPackageIdsParam
    logQuery countSql params
    let action conn = query conn countSql params
    result <- runDB action
    let count =
          case result of
            [count] -> fromOnly count
            _ -> 0
    -- 3. Get entities
    let sql =
          fromString $
            f''
              "WITH filtered_project AS (SELECT DISTINCT w_project.uuid, \
              \                             w_project.name, \
              \                             w_project.description, \
              \                             w_project.visibility, \
              \                             w_project.sharing, \
              \                             w_project.is_template, \
              \                             w_project.created_at, \
              \                             w_project.updated_at, \
              \                             w_project.knowledge_model_package_uuid, \
              \                             w_project.document_template_uuid \
              \             FROM w_project \
              \             ${knowledgeModelPackageJoin} \
              \             ${aclJoins} \
              \             WHERE w_project.tenant_uuid = '${tenantUuid}' ${aclCondition} ${nameCondition} ${isTemplateCondition} ${projectTagsCondition} ${userUuidsCondition} ${userGroupUuidsCondition} ${knowledgeModelPackageCondition}), \
              \     pkg AS (SELECT w_knowledge_model_package.uuid, \
              \                    w_knowledge_model_package.name, \
              \                    w_knowledge_model_package.version, \
              \                    w_knowledge_model_package.organization_id, \
              \                    w_knowledge_model_package.km_id \
              \             FROM w_knowledge_model_package \
              \             WHERE w_knowledge_model_package.tenant_uuid = '${tenantUuid}') \
              \SELECT  filtered_project.uuid, \
              \        filtered_project.name, \
              \        filtered_project.description, \
              \        filtered_project.visibility, \
              \        filtered_project.sharing, \
              \        filtered_project.is_template, \
              \        filtered_project.created_at, \
              \        filtered_project.updated_at, \
              \        CASE \
              \          WHEN filtered_project.knowledge_model_package_uuid != w_get_newest_knowledge_model_package(pkg.organization_id, pkg.km_id, '${tenantUuid}', ARRAY['ReleasedKnowledgeModelPackagePhase']) THEN 'OutdatedKnowledgeModelProjectState' \
              \          ELSE 'UpToDateKnowledgeModelProjectState' END, \
              \        CASE \
              \          WHEN dt.uuid IS NULL THEN NULL \
              \          WHEN dt.uuid != (SELECT newest_dt.uuid \
              \                           FROM w_document_template newest_dt \
              \                           WHERE newest_dt.tenant_uuid = '${tenantUuid}' \
              \                             AND newest_dt.organization_id = dt.organization_id \
              \                             AND newest_dt.template_id = dt.template_id \
              \                             AND newest_dt.phase = 'ReleasedDocumentTemplatePhase' \
              \                           ORDER BY split_part(newest_dt.version, '.', 1)::int DESC, \
              \                                    split_part(newest_dt.version, '.', 2)::int DESC, \
              \                                    split_part(newest_dt.version, '.', 3)::int DESC \
              \                           LIMIT 1) THEN 'OutdatedDocumentTemplateProjectState' \
              \          ELSE 'UpToDateDocumentTemplateProjectState' END, \
              \        pkg.uuid, \
              \        pkg.name, \
              \        pkg.version, \
              \       (SELECT array_agg(CONCAT(w_project_perm_user.user_uuid, '::', w_project_perm_user.perms, '::', u.uuid, '::', u.first_name, '::', u.last_name, '::', u.email, '::', u.image_url, '::', u.affiliation)) \
              \        FROM w_project_perm_user \
              \        JOIN w_user_entity u on u.uuid = w_project_perm_user.user_uuid \
              \        WHERE project_uuid = filtered_project.uuid \
              \        GROUP BY project_uuid) as user_permissions, \
              \       (SELECT array_agg(CONCAT(w_project_perm_group.user_group_uuid, '::', w_project_perm_group.perms, '::', ug.uuid, '::', ug.name, '::', ug.private, '::', ug.description)) \
              \        FROM w_project_perm_group \
              \        JOIN w_user_group ug on ug.uuid = w_project_perm_group.user_group_uuid \
              \        WHERE project_uuid = filtered_project.uuid \
              \        GROUP BY project_uuid) as group_permissions \
              \FROM filtered_project \
              \JOIN pkg ON filtered_project.knowledge_model_package_uuid = pkg.uuid \
              \LEFT JOIN w_document_template dt ON filtered_project.document_template_uuid = dt.uuid AND dt.tenant_uuid = '${tenantUuid}' \
              \${sort} \
              \OFFSET ${offset} LIMIT ${limit}"
              [ ("knowledgeModelPackageJoin", knowledgeModelPackageJoin)
              , ("aclJoins", aclJoins)
              , ("tenantUuid", U.toString tenantUuid)
              , ("aclCondition", aclCondition)
              , ("nameCondition", nameCondition)
              , ("isTemplateCondition", isTemplateCondition)
              , ("projectTagsCondition", projectTagsCondition)
              , ("userUuidsCondition", userUuidsCondition)
              , ("userGroupUuidsCondition", userGroupUuidsCondition)
              , ("knowledgeModelPackageCondition", knowledgeModelPackageCondition)
              , ("sort", mapSortWithPrefix "filtered_project" sort)
              , ("offset", show skip)
              , ("limit", show sizeI)
              ]
    logQuery sql params
    let action conn = query conn sql params
    entities <- runDB action
    -- 5. Constructor response
    let metadata =
          PageMetadata
            { size = sizeI
            , totalElements = count
            , totalPages = computeTotalPage count sizeI
            , number = pageI
            }
    return $ Page pageLabel metadata entities

findProjectsByKnowledgeModelPackageUuid :: U.UUID -> AppContextM [Project]
findProjectsByKnowledgeModelPackageUuid pkgUuid = do
  tenantUuid <- asks currentTenantUuid
  currentUser <- getCurrentUser
  hasPermission <- hasPermission _PROJECTS_VIEW_ROLE_PERMISSION
  if hasPermission
    then createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("knowledge_model_package_uuid", U.toString pkgUuid)] >>= traverse enhance
    else do
      let sql =
            fromString $
              f' (projectSelectSql (U.toString tenantUuid) (U.toString currentUser.uuid) "['VIEW']") ["AND knowledge_model_package_uuid = ?"]
      let params = [U.toString pkgUuid]
      logQuery sql params
      let action conn = query conn sql params
      entities <- runDB action
      traverse enhance entities

findProjectsByDocumentTemplateUuid :: U.UUID -> AppContextM [Project]
findProjectsByDocumentTemplateUuid documentTemplateUuid = do
  tenantUuid <- asks currentTenantUuid
  currentUser <- getCurrentUser
  hasPermission <- hasPermission _PROJECTS_VIEW_ROLE_PERMISSION
  if hasPermission
    then createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid)] >>= traverse enhance
    else do
      let sql =
            fromString $
              f' (projectSelectSql (U.toString tenantUuid) (U.toString currentUser.uuid) "['VIEW']") ["AND document_template_uuid = ?"]
      let params = [documentTemplateUuid]
      logQuery sql params
      let action conn = query conn sql params
      entities <- runDB action
      traverse enhance entities

findProjectsWithZeroAcl :: AppContextM [Project]
findProjectsWithZeroAcl = do
  let sql =
        f'
          "SELECT w_project.* \
          \FROM %s \
          \LEFT JOIN w_project_perm_user ON w_project.uuid = w_project_perm_user.project_uuid \
          \LEFT JOIN w_project_perm_group ON w_project.uuid = w_project_perm_group.project_uuid \
          \WHERE w_project_perm_user.user_uuid IS NULL \
          \AND w_project_perm_group.user_group_uuid IS NULL \
          \AND w_project.updated_at < now() - INTERVAL '30 days'"
          [entityName]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  runDB action

findProjectsSimpleWithPermByUserGroupUuid :: U.UUID -> AppContextM [ProjectSimpleWithPerm]
findProjectsSimpleWithPermByUserGroupUuid userGroupUuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "SELECT \
          \  nested_project.*, \
          \  ( \
          \    SELECT array_agg(CONCAT(user_uuid, '::', perms)) \
          \    FROM w_project_perm_user \
          \    WHERE project_uuid = nested_project.uuid AND tenant_uuid = nested_project.tenant_uuid \
          \    GROUP BY project_uuid \
          \  ) as user_permissions, \
          \  ( \
          \    SELECT array_agg(CONCAT(user_group_uuid, '::', perms)) \
          \    FROM w_project_perm_group \
          \    WHERE project_uuid = nested_project.uuid AND tenant_uuid = nested_project.tenant_uuid \
          \    GROUP BY project_uuid \
          \  ) as group_permissions \
          \FROM ( \
          \  SELECT w_project.uuid, w_project.visibility, w_project.sharing, w_project.tenant_uuid \
          \  FROM w_project \
          \  LEFT JOIN w_project_perm_group ON w_project.uuid = w_project_perm_group.project_uuid AND w_project.tenant_uuid = w_project_perm_group.tenant_uuid \
          \  WHERE w_project_perm_group.user_group_uuid = ? AND w_project_perm_group.tenant_uuid = ? \
          \) nested_project"
  let params = [toField userGroupUuid, toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findProjectByUuid :: U.UUID -> AppContextM Project
findProjectByUuid projectUuid = do
  tenantUuid <- asks currentTenantUuid
  entity <- createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString projectUuid)]
  enhance entity

findProjectByUuid' :: U.UUID -> AppContextM (Maybe Project)
findProjectByUuid' projectUuid = do
  tenantUuid <- asks currentTenantUuid
  mEntity <- createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString projectUuid)]
  case mEntity of
    Just entity -> enhance entity >>= return . Just
    Nothing -> return Nothing

findProjectSuggestionByUuid' :: U.UUID -> AppContextM (Maybe ProjectSuggestion)
findProjectSuggestionByUuid' uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityWithFieldsByFn' "uuid, name, description" entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findProjectForSquashing :: AppContextM [U.UUID]
findProjectForSquashing = do
  let sql = "SELECT uuid FROM w_project WHERE squashed = false"
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  entities <- runDB action
  return . concat $ entities

findProjectDetail :: U.UUID -> AppContextM ProjectDetail
findProjectDetail uuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString $
          f''
            "SELECT w_project.uuid, \
            \       w_project.name, \
            \       w_project.visibility, \
            \       w_project.sharing, \
            \       w_knowledge_model_package.uuid AS knowledge_model_package_uuid, \
            \       w_knowledge_model_package.name AS knowledge_model_package_name, \
            \       w_knowledge_model_package.organization_id AS knowledge_model_package_organization_id, \
            \       w_knowledge_model_package.km_id AS knowledge_model_package_km_id, \
            \       w_knowledge_model_package.version AS knowledge_model_package_version, \
            \       w_knowledge_model_package.description AS knowledge_model_package_description, \
            \       w_project.selected_question_tag_uuids, \
            \       w_project.is_template, \
            \       ${projectDetailPermSql}, \
            \       ( \
            \        SELECT count(*) \
            \        FROM w_project_file \
            \        WHERE tenant_uuid = '${tenantUuid}' AND project_uuid = '${projectUuid}' \
            \       ) as file_count \
            \FROM w_project \
            \LEFT JOIN w_knowledge_model_package ON w_project.knowledge_model_package_uuid = w_knowledge_model_package.uuid AND w_project.tenant_uuid = w_knowledge_model_package.tenant_uuid \
            \WHERE w_project.tenant_uuid = ? AND w_project.uuid = ?"
            [ ("projectUuid", U.toString uuid)
            , ("projectDetailPermSql", projectDetailPermSql)
            , ("tenantUuid", U.toString tenantUuid)
            ]
  let queryParams = [("tenant_uuid", U.toString tenantUuid), ("uuid", U.toString uuid)]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runOneEntityDB entityName action queryParams

findProjectDetailQuestionnaire :: U.UUID -> AppContextM ProjectDetailQuestionnaire
findProjectDetailQuestionnaire uuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString $
          f''
            "SELECT w_project.uuid, \
            \       w_project.name, \
            \       w_project.visibility, \
            \       w_project.sharing, \
            \       w_knowledge_model_package.uuid AS knowledge_model_package_uuid, \
            \       w_knowledge_model_package.name AS knowledge_model_package_name, \
            \       w_knowledge_model_package.organization_id AS knowledge_model_package_organization_id, \
            \       w_knowledge_model_package.km_id AS knowledge_model_package_km_id, \
            \       w_knowledge_model_package.version AS knowledge_model_package_version, \
            \       w_knowledge_model_package.description AS knowledge_model_package_description, \
            \       w_project.selected_question_tag_uuids, \
            \       w_project.language, \
            \       w_project.is_template, \
            \       ${projectDetailPermSql}, \
            \       ( \
            \        SELECT array_agg(concat(uuid, '<:::::>', \
            \                                file_name, '<:::::>', \
            \                                content_type, '<:::::>', \
            \                                file_size \
            \                        )) \
            \        FROM w_project_file \
            \        WHERE tenant_uuid = '${tenantUuid}' AND project_uuid = '${projectUuid}' \
            \       ) as files \
            \FROM w_project \
            \LEFT JOIN w_knowledge_model_package ON w_project.knowledge_model_package_uuid = w_knowledge_model_package.uuid AND w_project.tenant_uuid = w_knowledge_model_package.tenant_uuid \
            \WHERE w_project.tenant_uuid = ? AND w_project.uuid = ?"
            [ ("projectUuid", U.toString uuid)
            , ("projectDetailPermSql", projectDetailPermSql)
            , ("tenantUuid", U.toString tenantUuid)
            ]
  let queryParams = [("tenant_uuid", U.toString tenantUuid), ("uuid", U.toString uuid)]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runOneEntityDB entityName action queryParams

findProjectDetailPreview :: U.UUID -> AppContextM ProjectDetailPreview
findProjectDetailPreview uuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString $
          f''
            "SELECT w_project.uuid, \
            \       w_project.name, \
            \       w_project.visibility, \
            \       w_project.sharing, \
            \       w_knowledge_model_package.uuid AS knowledge_model_package_uuid, \
            \       w_knowledge_model_package.name AS knowledge_model_package_name, \
            \       w_knowledge_model_package.organization_id AS knowledge_model_package_organization_id, \
            \       w_knowledge_model_package.km_id AS knowledge_model_package_km_id, \
            \       w_knowledge_model_package.version AS knowledge_model_package_version, \
            \       w_knowledge_model_package.description AS knowledge_model_package_description, \
            \       w_project.is_template, \
            \       w_project.document_template_uuid, \
            \       ${projectDetailPermSql}, \
            \       dt_format.uuid, \
            \       dt_format.name, \
            \       dt_format.icon, \
            \       ( \
            \        SELECT count(*) \
            \        FROM w_project_file \
            \        WHERE tenant_uuid = '${tenantUuid}' AND project_uuid = '${projectUuid}' \
            \       ) as file_count \
            \FROM w_project \
            \LEFT JOIN w_knowledge_model_package ON w_project.knowledge_model_package_uuid = w_knowledge_model_package.uuid AND w_project.tenant_uuid = w_knowledge_model_package.tenant_uuid \
            \LEFT JOIN w_document_template dt ON w_project.document_template_uuid = dt.uuid AND w_project.tenant_uuid = dt.tenant_uuid \
            \LEFT JOIN w_document_template_format dt_format ON w_project.document_template_uuid = dt_format.document_template_uuid AND w_project.format_uuid = dt_format.uuid AND w_project.tenant_uuid = dt_format.tenant_uuid \
            \WHERE w_project.tenant_uuid = ? AND w_project.uuid = ?"
            [ ("projectDetailPermSql", projectDetailPermSql)
            , ("projectUuid", U.toString uuid)
            , ("tenantUuid", U.toString tenantUuid)
            ]
  let queryParams = [("tenant_uuid", U.toString tenantUuid), ("uuid", U.toString uuid)]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runOneEntityDB entityName action queryParams

findProjectDetailSettings :: U.UUID -> AppContextM ProjectDetailSettings
findProjectDetailSettings uuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString $
          f''
            "SELECT w_project.uuid, \
            \       w_project.name, \
            \       w_project.description, \
            \       w_project.visibility, \
            \       w_project.sharing, \
            \       w_project.is_template, \
            \       w_project.project_tags, \
            \       w_project.selected_question_tag_uuids, \
            \       w_project.language, \
            \       w_project.format_uuid, \
            \       ${projectDetailPermSql}, \
            \       pkg.uuid                       as knowledge_model_package_uuid, \
            \       pkg.name                       as knowledge_model_package_name, \
            \       pkg.organization_id            as knowledge_model_package_organization_id, \
            \       pkg.km_id                      as knowledge_model_package_km_id, \
            \       pkg.version                    as knowledge_model_package_version, \
            \       pkg.phase                      as knowledge_model_package_phase, \
            \       pkg.description                as knowledge_model_package_description, \
            \       pkg.non_editable               as knowledge_model_package_non_editable, \
            \       pkg.public                     as knowledge_model_package_public, \
            \       pkg.language                   as knowledge_model_package_language, \
            \       pkg.created_at                 as knowledge_model_package_created_at, \
            \       dt.uuid                        as document_template_uuid, \
            \       dt.name                        as document_template_name, \
            \       dt.organization_id             as document_template_organization_id, \
            \       dt.template_id                 as document_template_template_id, \
            \       dt.version                     as document_template_version, \
            \       dt.phase                       as document_template_phase, \
            \       dt.description                 as document_template_description, \
            \       ( \
            \        SELECT jsonb_agg(jsonb_build_object('uuid', uuid, 'name', name, 'icon', icon)) \
            \        FROM (SELECT * \
            \              FROM w_document_template_format dt_format \
            \              WHERE dt_format.tenant_uuid = w_project.tenant_uuid AND dt_format.document_template_uuid = dt.uuid \
            \              ORDER BY dt_format.name) nested \
            \       ) AS document_template_formats, \
            \       dt.metamodel_version           as document_template_metamodel_version, \
            \       CASE \
            \         WHEN w_project.knowledge_model_package_uuid != w_get_newest_knowledge_model_package(pkg.organization_id, pkg.km_id, '${tenantUuid}', ARRAY['ReleasedKnowledgeModelPackagePhase']) THEN 'OutdatedKnowledgeModelProjectState' \
            \         ELSE 'UpToDateKnowledgeModelProjectState' END as knowledge_model_state, \
            \       CASE \
            \         WHEN dt.uuid IS NULL THEN NULL \
            \         WHEN dt.uuid != (SELECT newest_dt.uuid \
            \                          FROM w_document_template newest_dt \
            \                          WHERE newest_dt.tenant_uuid = '${tenantUuid}' \
            \                            AND newest_dt.organization_id = dt.organization_id \
            \                            AND newest_dt.template_id = dt.template_id \
            \                            AND newest_dt.phase = 'ReleasedDocumentTemplatePhase' \
            \                          ORDER BY split_part(newest_dt.version, '.', 1)::int DESC, \
            \                                   split_part(newest_dt.version, '.', 2)::int DESC, \
            \                                   split_part(newest_dt.version, '.', 3)::int DESC \
            \                          LIMIT 1) THEN 'OutdatedDocumentTemplateProjectState' \
            \         ELSE 'UpToDateDocumentTemplateProjectState' END as document_template_state, \
            \       ( \
            \        SELECT count(*) \
            \        FROM w_project_file \
            \        WHERE tenant_uuid = '${tenantUuid}' AND project_uuid = '${projectUuid}' \
            \       ) as file_count \
            \FROM w_project \
            \LEFT JOIN w_knowledge_model_package pkg ON w_project.knowledge_model_package_uuid = pkg.uuid AND w_project.tenant_uuid = pkg.tenant_uuid \
            \LEFT JOIN w_document_template dt ON w_project.document_template_uuid = dt.uuid AND w_project.tenant_uuid = dt.tenant_uuid \
            \WHERE w_project.tenant_uuid = ? AND w_project.uuid = ?"
            [ ("projectDetailPermSql", projectDetailPermSql)
            , ("projectUuid", U.toString uuid)
            , ("tenantUuid", U.toString tenantUuid)
            ]
  let queryParams = [("tenant_uuid", U.toString tenantUuid), ("uuid", U.toString uuid)]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runOneEntityDB entityName action queryParams

projectDetailPermSql :: String
projectDetailPermSql =
  "(SELECT array_agg(CONCAT(w_project_perm_user.user_uuid, '::', w_project_perm_user.perms, '::', u.uuid, '::', u.first_name, \
  \                         '::', u.last_name, '::', u.email, '::', u.image_url, '::', u.affiliation)) \
  \ FROM w_project_perm_user \
  \          JOIN w_user_entity u on u.uuid = w_project_perm_user.user_uuid \
  \ WHERE project_uuid = w_project.uuid \
  \ GROUP BY project_uuid)  as user_permissions, \
  \(SELECT array_agg(CONCAT(w_project_perm_group.user_group_uuid, '::', w_project_perm_group.perms, '::', ug.uuid, '::', ug.name, \
  \                         '::', ug.private, '::', ug.description)) \
  \ FROM w_project_perm_group \
  \          JOIN w_user_group ug on ug.uuid = w_project_perm_group.user_group_uuid \
  \ WHERE project_uuid = w_project.uuid \
  \ GROUP BY project_uuid)  as group_permissions"

countProjects :: AppContextM Int
countProjects = do
  tenantUuid <- asks currentTenantUuid
  countProjectsWithTenant tenantUuid

countProjectsWithTenant :: U.UUID -> AppContextM Int
countProjectsWithTenant tenantUuid = createCountByFn entityName tenantCondition [U.toString tenantUuid]

insertProject :: Project -> AppContextM Int64
insertProject project = do
  -- Insert project
  let sql =
        fromString
          "INSERT INTO w_project VALUES (?, ?, ?, ?, ?, ?::uuid[], ?, ?, ?, ?, ?, ?, ?, ?, ?, ?::text[], ?)"
  let params = toRow project
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  -- Insert project permissions
  traverse_ insertProjectPerm project.permissions
  return 1

updateProjectByUuid :: Project -> AppContextM ()
updateProjectByUuid project = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "UPDATE w_project SET uuid = ?, name = ?, visibility = ?, sharing = ?, knowledge_model_package_uuid = ?, selected_question_tag_uuids = ?::uuid[], document_template_uuid = ?, format_uuid = ?, created_by = ?, created_at = ?, updated_at = ?, description = ?, is_template = ?, squashed = ?, tenant_uuid = ?, project_tags = ?::text[], language = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = toRow project ++ [toField tenantUuid, toField . U.toText $ project.uuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  deleteProjectPermsFiltered [("project_uuid", U.toString project.uuid)]
  traverse_ insertProjectPerm project.permissions

updateProjectSquashedByUuid :: U.UUID -> Bool -> AppContextM Int64
updateProjectSquashedByUuid uuid squashed = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE w_project SET squashed = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField squashed, toField tenantUuid, toField . U.toText $ uuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

updateProjectSquashedAndUpdatedAtByUuid :: U.UUID -> Bool -> UTCTime -> AppContextM Int64
updateProjectSquashedAndUpdatedAtByUuid uuid squashed updatedAt = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE w_project SET squashed = ?, updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField squashed, toField updatedAt, toField tenantUuid, toField . U.toText $ uuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

updateProjectUpdatedAtByUuid :: U.UUID -> AppContextM Int64
updateProjectUpdatedAtByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE w_project SET updated_at = now() WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField tenantUuid, toField . U.toText $ uuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

deleteProjects :: AppContextM Int64
deleteProjects = createDeleteEntitiesFn entityName

deleteProjectsFiltered :: [(String, String)] -> AppContextM Int64
deleteProjectsFiltered params = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntitiesByFn entityName (tenantQueryUuid tenantUuid : params)

deleteProjectByUuid :: U.UUID -> AppContextM Int64
deleteProjectByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

-- ------------------------------------------------------------------------------------------------------------------------------
-- PRIVATE
-- ------------------------------------------------------------------------------------------------------------------------------
projectSelectSql tenantUuid userUuid perm =
  f'
    "SELECT w_project.* \
    \FROM w_project \
    \LEFT JOIN w_project_perm_user ON w_project.uuid = w_project_perm_user.project_uuid \
    \LEFT JOIN w_project_perm_group ON w_project.uuid = w_project_perm_group.project_uuid \
    \WHERE %s %s"
    [projectWhereSql tenantUuid userUuid perm]

projectWhereSql tenantUuid userUuid perm =
  f'
    "w_project.tenant_uuid = '%s' \
    \AND (visibility = 'VisibleEditProjectVisibility' \
    \OR visibility = 'VisibleCommentProjectVisibility' \
    \OR visibility = 'VisibleViewProjectVisibility' \
    \OR (visibility = 'PrivateProjectVisibility' AND w_project_perm_user.user_uuid = '%s' AND w_project_perm_user.perms @> ARRAY %s))"
    [tenantUuid, userUuid, perm]

enhance :: Project -> AppContextM Project
enhance project = do
  ps <- findProjectPermsFiltered [("project_uuid", U.toString project.uuid)]
  return $ project {permissions = ps}
