module Specs.Service.Project.Cache.ProjectCacheServiceSpec where

import Data.Foldable (traverse_)
import Data.Maybe (isJust)
import Test.Hspec

import Shared.Constant.Tenant
import Shared.Util.Date

import Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Shared.Api.Resource.Project.Detail.ProjectDetailReportDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelCacheDAO
import Shared.Database.DAO.Project.ProjectCacheDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.Project.ProjectVersionDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KM_PKG_Migration
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.ProjectVersions
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.Cache.ProjectCache
import Shared.Model.Project.Project
import Shared.Model.Project.Version.ProjectVersion
import Shared.Model.Project.Version.ProjectVersionList
import Shared.Service.Project.Cache.ProjectCacheService
import Shared.Service.Project.ProjectService
import Shared.Service.Project.Version.ProjectVersionService
import Shared.Service.Report.ReportService

import Specs.Common

projectCacheServiceSpec requestContext =
  describe "Project Cache Service" $ do
    it "refreshProjectCaches caches the questionnaire, report and versions of every project" $
      -- GIVEN:
      do
        runInContextIO TML_Migration.runMigration requestContext
        runInContextIO PRJ_Migration.runMigration requestContext
        -- WHEN:
        (Right []) <- runInContext refreshProjectCaches requestContext
        -- THEN:
        (Right projects) <- runInContext findProjects requestContext
        (Right caches) <- runInContext findProjectCaches requestContext
        length caches `shouldBe` length projects
        (Right (Just cache)) <- runInContext (findProjectCacheByProjectUuid' project1Uuid) requestContext
        (Right questionnaire) <- runInContext (getProjectDetailQuestionnaireByUuid project1Uuid) requestContext
        (Right report) <- runInContext (getReportByProjectUuid project1Uuid) requestContext
        (Right versions) <- runInContext (getVersions project1Uuid) requestContext
        cache.questionnaire `shouldBe` questionnaire
        cache.report.totalReport `shouldBe` report.totalReport
        cache.report.chapterReports `shouldBe` report.chapterReports
        cache.versions `shouldBe` versions
        (Right mKmCache) <- runInContext (findKnowledgeModelCacheByUuid' project1.knowledgeModelPackageUuid project1.selectedQuestionTagUuids defaultTenantUuid) requestContext
        isJust mKmCache `shouldBe` True

    it "refreshProjectCaches leaves an unchanged project alone" $
      -- GIVEN:
      do
        runInContextIO TML_Migration.runMigration requestContext
        runInContextIO PRJ_Migration.runMigration requestContext
        runInContextIO refreshProjectCaches requestContext
        (Right (Just cacheBefore)) <- runInContext (findProjectCacheByProjectUuid' project1Uuid) requestContext
        -- WHEN:
        (Right []) <- runInContext refreshProjectCaches requestContext
        -- THEN:
        (Right (Just cacheAfter)) <- runInContext (findProjectCacheByProjectUuid' project1Uuid) requestContext
        cacheAfter `shouldBe` cacheBefore

    it "refreshProjectCaches recomputes the project after a new reply" $
      -- GIVEN:
      do
        runInContextIO TML_Migration.runMigration requestContext
        runInContextIO PRJ_Migration.runMigration requestContext
        runInContextIO refreshProjectCaches requestContext
        (Right (Just cacheBefore)) <- runInContext (findProjectCacheByProjectUuid' project1Uuid) requestContext
        runInContextIO (insertProjectEventWithTimestampUpdate project1Uuid (sre_rQ1Updated' project1Uuid)) requestContext
        -- WHEN:
        (Right []) <- runInContext refreshProjectCaches requestContext
        -- THEN:
        (Right (Just cacheAfter)) <- runInContext (findProjectCacheByProjectUuid' project1Uuid) requestContext
        cacheAfter.questionnaire.replies `shouldNotBe` cacheBefore.questionnaire.replies
        (cacheAfter.questionnaireSourceUpdatedAt > cacheBefore.questionnaireSourceUpdatedAt) `shouldBe` True
        cacheAfter.versions `shouldBe` cacheBefore.versions
        (cacheAfter.versionsSourceUpdatedAt > cacheBefore.versionsSourceUpdatedAt) `shouldBe` True

    it "refreshProjectCaches recomputes only the versions after a version rename" $
      -- GIVEN:
      do
        runInContextIO TML_Migration.runMigration requestContext
        runInContextIO PRJ_Migration.runMigration requestContext
        runInContextIO refreshProjectCaches requestContext
        (Right (Just cacheBefore)) <- runInContext (findProjectCacheByProjectUuid' project1Uuid) requestContext
        let now = dt' 2030 1 1
        runInContextIO (updateProjectVersionByUuid ((projectVersion1 project1Uuid) {name = "Renamed", updatedAt = now} :: ProjectVersion)) requestContext
        -- WHEN:
        (Right []) <- runInContext refreshProjectCaches requestContext
        -- THEN:
        (Right (Just cacheAfter)) <- runInContext (findProjectCacheByProjectUuid' project1Uuid) requestContext
        fmap (.name) cacheAfter.versions `shouldBe` ["Renamed"]
        cacheAfter.versionsSourceUpdatedAt `shouldBe` now
        cacheAfter.questionnaire `shouldBe` cacheBefore.questionnaire
        cacheAfter.questionnaireSourceUpdatedAt `shouldBe` cacheBefore.questionnaireSourceUpdatedAt

    it "refreshProjectCaches recomputes the versions after a version delete" $
      -- GIVEN:
      do
        runInContextIO TML_Migration.runMigration requestContext
        runInContextIO PRJ_Migration.runMigration requestContext
        runInContextIO refreshProjectCaches requestContext
        runInContextIO (deleteVersion project1Uuid (projectVersion1 project1Uuid).uuid) requestContext
        -- WHEN:
        (Right []) <- runInContext refreshProjectCaches requestContext
        -- THEN:
        (Right (Just cacheAfter)) <- runInContext (findProjectCacheByProjectUuid' project1Uuid) requestContext
        cacheAfter.versions `shouldBe` []

    it "refreshProjectCache re-creates a cache row deleted after the sources were loaded" $
      -- GIVEN:
      do
        runInContextIO TML_Migration.runMigration requestContext
        runInContextIO PRJ_Migration.runMigration requestContext
        runInContextIO refreshProjectCaches requestContext
        runInContextIO (insertProjectEventWithTimestampUpdate project1Uuid (sre_rQ1Updated' project1Uuid)) requestContext
        (Right sources) <- runInContext findOutdatedProjectCacheSources requestContext
        runInContextIO (deleteProjectCacheByProjectUuid project1Uuid) requestContext
        -- WHEN:
        runInContextIO (traverse_ refreshProjectCache sources) requestContext
        -- THEN:
        (Right mCache) <- runInContext (findProjectCacheByProjectUuid' project1Uuid) requestContext
        isJust mCache `shouldBe` True

    it "refreshProjectCaches keeps a version whose creator was removed" $
      -- GIVEN:
      do
        runInContextIO TML_Migration.runMigration requestContext
        runInContextIO PRJ_Migration.runMigration requestContext
        runInContextIO (updateProjectVersionByUuid ((projectVersion1 project1Uuid) {createdBy = Nothing} :: ProjectVersion)) requestContext
        -- WHEN:
        runInContextIO refreshProjectCaches requestContext
        -- THEN:
        (Right (Just cache)) <- runInContext (findProjectCacheByProjectUuid' project1Uuid) requestContext
        fmap (.uuid) cache.versions `shouldBe` [(projectVersion1 project1Uuid).uuid]

    it "refreshProjectCaches caches the knowledge model of a package no project uses" $
      -- GIVEN:
      do
        runInContextIO TML_Migration.runMigration requestContext
        runInContextIO KM_PKG_Migration.runMigration requestContext
        runInContextIO PRJ_Migration.runMigration requestContext
        -- WHEN:
        (Right []) <- runInContext refreshProjectCaches requestContext
        -- THEN:
        (Right mKmCache) <- runInContext (findKnowledgeModelCacheByUuid' globalKmPackage.uuid [] defaultTenantUuid) requestContext
        isJust mKmCache `shouldBe` True
