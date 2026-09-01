module Shared.Service.Project.Cache.ProjectCacheService where

import Control.Monad (void, when)
import Control.Monad.Except (catchError)
import Control.Monad.Reader (asks, liftIO)
import Data.Maybe (catMaybes)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Shared.Api.Resource.Project.Detail.ProjectDetailReportDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelCacheDAO
import Shared.Database.DAO.Project.ProjectCacheDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectVersionDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Project.Cache.ProjectCache
import Shared.Model.Project.Cache.ProjectCacheSource
import Shared.Service.KnowledgeModel.KnowledgeModelService
import Shared.Service.Project.ProjectService
import Shared.Service.Report.ReportService
import Shared.Util.Logger

refreshProjectCaches :: WizardRequestContextC s m => m [String]
refreshProjectCaches = do
  missingKeys <- findMissingKnowledgeModelCacheKeys
  kmFailures <- traverse (\(pkgUuid, tagUuids) -> collectFailure "knowledge model" pkgUuid . void $ compileKnowledgeModel [] (Just pkgUuid) tagUuids) missingKeys
  sources <- findOutdatedProjectCacheSources
  projectFailures <- traverse (\source -> collectFailure "project" source.projectUuid (refreshProjectCache source)) sources
  return . catMaybes $ kmFailures ++ projectFailures

collectFailure :: WizardRequestContextC s m => String -> U.UUID -> m () -> m (Maybe String)
collectFailure entity uuid action =
  (action >> return Nothing) `catchError` \error -> do
    let message = f' "Cache of %s '%s' could not be refreshed: %s" [entity, U.toString uuid, show error]
    logWarnI _CMP_SERVICE message
    return . Just $ message

refreshProjectCache :: WizardRequestContextC s m => ProjectCacheSource -> m ()
refreshProjectCache source =
  runInTransaction $ do
    now <- liftIO getCurrentTime
    mCachedSourceUpdatedAt <- findProjectCacheWatermarksByProjectUuid' source.projectUuid
    case mCachedSourceUpdatedAt of
      Just (cachedQuestionnaireSourceUpdatedAt, cachedVersionsSourceUpdatedAt) -> do
        when (cachedQuestionnaireSourceUpdatedAt < source.questionnaireSourceUpdatedAt) $ do
          (questionnaire, report) <- computeQuestionnaireAndReport source.projectUuid
          void $ updateProjectCacheQuestionnaireByProjectUuid source.projectUuid questionnaire report source.questionnaireSourceUpdatedAt now
        when (cachedVersionsSourceUpdatedAt < source.versionsSourceUpdatedAt) $ do
          versions <- findProjectVersionListByProjectUuidAndCreatedAt source.projectUuid Nothing
          void $ updateProjectCacheVersionsByProjectUuid source.projectUuid versions source.versionsSourceUpdatedAt now
      Nothing -> do
        (questionnaire, report) <- computeQuestionnaireAndReport source.projectUuid
        versions <- findProjectVersionListByProjectUuidAndCreatedAt source.projectUuid Nothing
        tenantUuid <- asks (.tenantUuid')
        void $
          insertProjectCache
            ProjectCache
              { projectUuid = source.projectUuid
              , questionnaire = questionnaire
              , report = report
              , versions = versions
              , questionnaireSourceUpdatedAt = source.questionnaireSourceUpdatedAt
              , versionsSourceUpdatedAt = source.versionsSourceUpdatedAt
              , tenantUuid = tenantUuid
              , createdAt = now
              , updatedAt = now
              }

computeQuestionnaireAndReport :: WizardRequestContextC s m => U.UUID -> m (ProjectDetailQuestionnaireDTO, ProjectDetailReportDTO)
computeQuestionnaireAndReport projectUuid = do
  project <- findProjectDetailQuestionnaire projectUuid
  questionnaire <- compileProjectDetailQuestionnaire project True True
  report <- generateProjectReport questionnaire
  return (questionnaire, report)
