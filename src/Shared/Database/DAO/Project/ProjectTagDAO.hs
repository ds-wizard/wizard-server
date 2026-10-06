module Shared.Database.DAO.Project.ProjectTagDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U

import Shared.Database.DAO.WizardCommon
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger
import Shared.Util.String (replace)

pageLabel = "projectTags"

findProjectTagsPage :: WizardRequestContextC s m => Maybe String -> [String] -> Pageable -> [Sort] -> m (Page String)
findProjectTagsPage mQuery excludeTags pageable sort =
  -- 1. Prepare variables
  do
    tenantUuid <- asks (.tenantUuid')
    workspaceCondition <- workspaceOnlyCondition Nothing "nested.workspace_uuid"
    let params = [U.toString tenantUuid, U.toString tenantUuid, regexM mQuery] ++ excludeTags
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    count <- findCount workspaceCondition excludeTags params
    -- 3. Prepare SQL
    let sql =
          fromString $
            f'
              "SELECT * \
              \FROM (%s) merged \
              \WHERE merged.project_tag ~* ? %s \
              \%s \
              \OFFSET %s LIMIT %s"
              [sqlBase workspaceCondition, excludeTagsCondition excludeTags, mapSort sort, show skip, show sizeI]
    createFindColumnBySqlPageFn pageLabel pageable sql params count

findCount :: WizardRequestContextC s m => String -> [String] -> [String] -> m Int
findCount workspaceCondition excludeTags params = do
  let sql =
        fromString $
          f'
            "SELECT COUNT(*) \
            \FROM (%s) merged \
            \WHERE merged.project_tag::text ~* ? %s"
            [sqlBase workspaceCondition, excludeTagsCondition excludeTags]
  createCountWithSqlFn sql params

sqlBase :: String -> String
sqlBase workspaceCondition =
  f'
    "SELECT unnest(project_tagging_tags) as project_tag \
    \FROM settings_projects \
    \WHERE tenant_uuid = ? AND (workspace_uuid IS NULL OR (workspace_uuid IS NOT NULL%s)) \
    \UNION \
    \SELECT nested.project_tag \
    \FROM (SELECT unnest(project_tags) as project_tag, tenant_uuid, workspace_uuid FROM project) nested \
    \WHERE nested.tenant_uuid = ?%s "
    [replace "nested.workspace_uuid" "settings_projects.workspace_uuid" workspaceCondition, workspaceCondition]

excludeTagsCondition :: [String] -> String
excludeTagsCondition excludeTags =
  if null excludeTags
    then ""
    else f' "AND NOT (project_tag IN (%s))" [generateQuestionMarks excludeTags]
