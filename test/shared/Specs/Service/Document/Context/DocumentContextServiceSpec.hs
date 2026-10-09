module Specs.Service.Document.Context.DocumentContextServiceSpec where

import Data.Aeson (Value (..), toJSON)
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Time
import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.Project.ProjectFileDAO
import Shared.Database.DAO.Project.ProjectVersionDAO
import Shared.Database.Migration.Development.Document.Data.Documents
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateLocales
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KME
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.ProjectFiles
import Shared.Database.Migration.Development.Project.Data.ProjectReplies
import Shared.Database.Migration.Development.Project.Data.ProjectVersions
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as USR
import Shared.Localization.Messages.Public
import Shared.Model.Common.Lens
import Shared.Model.Document.Document
import Shared.Model.Document.DocumentContext
import Shared.Model.Document.DocumentContextJM ()
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.Event.ProjectEvent
import Shared.Model.Project.Event.ProjectEventLenses ()
import Shared.Model.Project.Project
import Shared.Model.Project.Version.ProjectVersion
import Shared.Model.Project.Version.ProjectVersionList
import Shared.Model.User.User
import Shared.Model.User.UserGroup
import Shared.Service.Document.Context.DocumentContextHash
import Shared.Service.Document.Context.DocumentContextMapper
import Shared.Service.Document.Context.DocumentContextService
import Shared.Util.Uuid

import Specs.Api.Handler.Common
import Specs.Common

documentContextServiceSpec requestContext =
  describe "DocumentContextService" $ do
    describe "createProjectDocumentContext" $ do
      it "builds the project context" $ do
        runProjectMigrations requestContext
        (ctx, cutOff) <- buildProjectContext requestContext project1 Nothing
        cutOff `shouldBe` Just (getUuid . last $ project1Events)
        ctx.metamodelVersion `shouldBe` dmp1.metamodelVersion
        ctx.workspace `shouldBe` dmp1.workspace
        let project = fromJust ctx.project
        project.uuid `shouldBe` dmp1Project.uuid
        project.name `shouldBe` dmp1Project.name
        project.users `shouldBe` dmp1Project.users
        project.groups `shouldBe` dmp1Project.groups
        ctx.knowledgeModelEditor `shouldBe` Nothing
        ctx.questionnaire.replies `shouldBe` dmp1Questionnaire.replies
        ctx.questionnaire.labels `shouldBe` dmp1Questionnaire.labels
        ctx.questionnaire.phaseUuid `shouldBe` dmp1Questionnaire.phaseUuid
        ctx.questionnaire.versionUuid `shouldBe` dmp1Questionnaire.versionUuid
        ctx.questionnaire.versions `shouldBe` dmp1Questionnaire.versions
        ctx.questionnaire.files `shouldBe` dmp1Questionnaire.files
        ctx.questionnaire.report.chapterReports `shouldBe` dmp1Questionnaire.report.chapterReports
        ctx.knowledgeModel `shouldBe` dmp1.knowledgeModel
        ctx.knowledgeModelPackage `shouldBe` Just dmp1Package
        ctx.document `shouldBe` Nothing
        ctx.hash `shouldBe` computeContextHash ctx
        take 7 ctx.hash `shouldBe` "sha256:"
        jsonKeys (toJSON ctx) `shouldBe` contextKeys
        jsonKeys (toJSON project) `shouldBe` projectKeys
        jsonKeys (toJSON ctx.questionnaire) `shouldBe` questionnaireKeys
        jsonField "metamodelVersion" (toJSON ctx) `shouldBe` String "19.0"
      it "sorts users, groups, perms and members" $ do
        runProjectMigrations requestContext
        let project = project1 {permissions = [nikolaPerm, bioGroupPerm, project1AlbertEditProjectPerm]} :: Project
        (ctx, _) <- buildProjectContext requestContext project Nothing
        let contextProject = fromJust ctx.project
        fmap (\u -> (u.user.uuid, u.perms)) contextProject.users `shouldBe` L.sort [(userAlbert.uuid, L.sort ownerPermissions), (userNikola.uuid, L.sort viewerPermissions)]
        fmap (\g -> g.group.uuid) contextProject.groups `shouldBe` [bioGroup.uuid]
        fmap (.perms) contextProject.groups `shouldBe` [L.sort editorPermissions]
        fmap (.uuid) (head contextProject.groups).group.members `shouldBe` L.sort [userAlbert.uuid, userNikola.uuid]
      it "changes the hash after a label change" $ do
        runProjectMigrations requestContext
        (before, _) <- buildProjectContext requestContext project1 Nothing
        runInContextIO (insertProjectEvent (slble_rQ2' project1.uuid)) requestContext
        (after, _) <- buildProjectContext requestContext project1 Nothing
        after.hash `shouldNotBe` before.hash
      it "changes the hash after a file insert" $ do
        runProjectMigrations requestContext
        (before, _) <- buildProjectContext requestContext project1 Nothing
        runInContextIO (insertProjectFile projectFile) requestContext
        (after, _) <- buildProjectContext requestContext project1 Nothing
        after.questionnaire.files `shouldBe` [projectFileSimple]
        after.hash `shouldNotBe` before.hash
      it "changes the hash after a collaborator change" $ do
        runProjectMigrations requestContext
        (before, _) <- buildProjectContext requestContext project1 Nothing
        (after, _) <- buildProjectContext requestContext (project1 {permissions = [project1AlbertEditProjectPerm, nikolaPerm]} :: Project) Nothing
        after.hash `shouldNotBe` before.hash
      it "keeps the hash when a document is attached" $ do
        runProjectMigrations requestContext
        (ctx, _) <- buildProjectContext requestContext project1 Nothing
        let withDoc = withDocument doc1 wizardDocumentTemplate formatJsonSimple (Just userNikola) (Just czechWizardDocumentTemplateLocale) ctx
        fmap (.uuid) withDoc.document `shouldBe` Just doc1.uuid
        (withDoc.document >>= (.createdBy)) `shouldBe` Just (toDocumentContextUser userNikola)
        fmap (.code) (withDoc.document >>= (.locale)) `shouldBe` Just "cs"
        withDoc.hash `shouldBe` ctx.hash
        computeContextHash withDoc `shouldBe` ctx.hash
      it "replays the project to an older event" $ do
        runProjectMigrations requestContext
        runInContextIO (insertProjectVersion laterVersion) requestContext
        let eventUuid = (sre_rQ2 project1.uuid).uuid
        (latest, _) <- buildProjectContext requestContext project1 Nothing
        (replayed, cutOff) <- buildProjectContext requestContext project1 (Just eventUuid)
        cutOff `shouldBe` Just eventUuid
        replayed.questionnaire.replies `shouldBe` M.fromList [rQ1, rQ2]
        replayed.questionnaire.phaseUuid `shouldBe` Nothing
        replayed.questionnaire.labels `shouldBe` M.empty
        replayed.questionnaire.versionUuid `shouldBe` Nothing
        fmap (.uuid) replayed.questionnaire.versions `shouldBe` [laterVersion.uuid]
        fmap (.uuid) latest.questionnaire.versions `shouldBe` [(projectVersion1 project1.uuid).uuid, laterVersion.uuid]
        replayed.hash `shouldNotBe` latest.hash
      it "fails for an unknown event" $ do
        runProjectMigrations requestContext
        let eventUuid = u' "e4f59b09-4b6a-4d4f-9b19-3d8c6f1a2b3c"
        result <- runInContext (createProjectDocumentContext project1 (Just eventUuid)) requestContext
        fmap fst result `shouldBe` Left (NotExistsError (_ERROR_DATABASE__ENTITY_NOT_FOUND "project_event" [("uuid", show eventUuid)]))
    describe "createKnowledgeModelEditorDocumentContext" $ do
      it "builds the editor context" $ do
        runInContextIO KME.runMigration requestContext
        ctx <- buildEditorContext requestContext amsterdamKnowledgeModelEditor
        ctx.project `shouldBe` Nothing
        let editor = fromJust ctx.knowledgeModelEditor
        editor.uuid `shouldBe` amsterdamKnowledgeModelEditor.uuid
        editor.dckeId `shouldBe` amsterdamKnowledgeModelEditor.id
        editor.previousPackageId `shouldBe` Just netherlandsKmPackage.id
        editor.previousPackageVersion `shouldBe` Just netherlandsKmPackage.version
        fmap (.uuid) editor.createdBy `shouldBe` Just userAlbert.uuid
        fmap (.uuid) ctx.knowledgeModelPackage `shouldBe` Just netherlandsKmPackage.uuid
        ctx.questionnaire.labels `shouldBe` M.empty
        ctx.questionnaire.phaseUuid `shouldBe` Nothing
        ctx.questionnaire.versionUuid `shouldBe` Nothing
        ctx.questionnaire.versions `shouldBe` []
        ctx.questionnaire.files `shouldBe` []
        ctx.questionnaire.language `shouldBe` Just "en"
        ctx.document `shouldBe` Nothing
        ctx.hash `shouldBe` computeContextHash ctx
        jsonKeys (toJSON ctx) `shouldBe` contextKeys
        jsonKeys (toJSON editor) `shouldBe` editorKeys
      it "has no package without a previous package" $ do
        runInContextIO (insertKnowledgeModelEditor editorWithoutPackage) requestContext
        ctx <- buildEditorContext requestContext editorWithoutPackage
        ctx.knowledgeModelPackage `shouldBe` Nothing
        (fromJust ctx.knowledgeModelEditor).previousPackageId `shouldBe` Nothing
        ctx.questionnaire.language `shouldBe` Nothing
      it "gives two editors different hashes" $ do
        runInContextIO KME.runMigration requestContext
        runInContextIO (insertKnowledgeModelEditor amsterdamKnowledgeModelEditorCopy) requestContext
        first <- buildEditorContext requestContext amsterdamKnowledgeModelEditor
        second <- buildEditorContext requestContext amsterdamKnowledgeModelEditorCopy
        second.hash `shouldNotBe` first.hash

-- --------------------------------
-- HELPERS
-- --------------------------------
runProjectMigrations requestContext = do
  runInContextIO USR.runMigration requestContext
  runInContextIO TML.runMigration requestContext
  runInContextIO PRJ.runMigration requestContext
  runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext

buildProjectContext requestContext project mEventUuid = do
  (Right result) <- runInContext (createProjectDocumentContext project mEventUuid) requestContext
  return result

buildEditorContext requestContext editor = do
  (Right result) <- runInContext (createKnowledgeModelEditorDocumentContext editor) requestContext
  return result

nikolaPerm :: ProjectPerm
nikolaPerm = project1AlbertEditProjectPerm {memberUuid = userNikola.uuid, perms = viewerPermissions}

bioGroupPerm :: ProjectPerm
bioGroupPerm = project1AlbertEditProjectPerm {memberType = UserGroupProjectPermType, memberUuid = bioGroup.uuid, perms = editorPermissions}

laterVersion :: ProjectVersion
laterVersion = (projectVersion2 project1.uuid) {eventUuid = (sre_rQ1 project1.uuid).uuid, createdAt = UTCTime (fromGregorian 2018 1 22) 0} :: ProjectVersion

editorWithoutPackage :: KnowledgeModelEditor
editorWithoutPackage = amsterdamKnowledgeModelEditor {uuid = u' "0d3b8f9a-2c4e-4f6a-8b1c-5d7e9f0a1b2c", previousPackageUuid = Nothing, language = ""} :: KnowledgeModelEditor

projectKeys :: [String]
projectKeys = L.sort ["uuid", "name", "description", "createdBy", "createdAt", "updatedAt", "visibility", "sharing", "isTemplate", "knowledgeModelPackageUuid", "selectedQuestionTagUuids", "projectTags", "documentTemplateUuid", "formatUuid", "users", "groups"]

questionnaireKeys :: [String]
questionnaireKeys = L.sort ["replies", "labels", "phaseUuid", "versionUuid", "versions", "files", "report", "language"]

editorKeys :: [String]
editorKeys = L.sort ["uuid", "name", "description", "createdBy", "createdAt", "updatedAt", "id", "version", "license", "readme", "previousPackageUuid", "previousPackageId", "previousPackageVersion", "knowledgeModelMetamodelVersion"]
