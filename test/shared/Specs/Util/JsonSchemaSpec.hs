module Specs.Util.JsonSchemaSpec where

import Data.Aeson
import Data.Either (isLeft)
import Data.JSON.JSONSchema (validate)
import qualified Data.Map.Strict as M
import Data.Proxy
import Test.Hspec

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM ()
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeJM ()
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDetailJM ()
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftListJM ()
import Shared.Api.Resource.DocumentTemplate.WizardDocumentTemplateSimpleJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorListJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Api.Resource.Mcp.DocumentPreview.McpDocumentPreviewJM ()
import Shared.Api.Resource.Mcp.Project.McpProjectJM ()
import Shared.Api.Resource.Mcp.Workspace.McpWorkspaceMembersDTO
import Shared.Api.Resource.Mcp.Workspace.McpWorkspaceMembersJM ()
import Shared.Api.Resource.Project.ProjectCreateJM ()
import Shared.Api.Resource.Project.ProjectReplyJM ()
import Shared.Api.Resource.Project.ProjectSettingsChangeJM ()
import Shared.Api.Resource.Settings.SettingsDTO
import Shared.Api.Resource.Settings.SettingsJM ()
import Shared.Api.Resource.User.Group.UserGroupDetailJM ()
import Shared.Api.Resource.User.Group.UserGroupListJM ()
import Shared.Api.Resource.User.UserJM ()
import Shared.Api.Resource.Workspace.WorkspaceChangeJM ()
import Shared.Api.Resource.Workspace.WorkspaceJM ()
import Shared.Constant.Mcp.KnowledgeModelEventExamples
import Shared.Database.Migration.Development.Document.Data.Documents
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.DocumentTemplate.Data.WizardDocumentTemplates
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.ProjectReplies
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Database.Migration.Development.User.Data.WizardAlbertEinstein
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.User.Role
import Shared.Model.User.User
import Shared.Service.DocumentTemplate.DocumentTemplateMapper (toFormatDTO)
import Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper
import Shared.Service.Mcp.Tool.DocumentTemplatePreviewMapper
import Shared.Service.Project.ProjectMapper (toMcpProjectDTO, toSuggestion)
import qualified Shared.Service.User.Group.UserGroupMapper as UserGroupMapper
import Shared.Util.JsonSchema

jsonSchemaSpec :: Spec
jsonSchemaSpec =
  describe "JsonSchema" $
    describe "development data validates and reads back" $ do
      it "KnowledgeModelEditorList" $ assertSchema amsterdamKnowledgeModelEditorList
      it "Chapter" $ mapM_ assertSchema (M.elems km1WithQ4.entities.chapters)
      it "Question" $ mapM_ assertSchema (M.elems km1WithQ4.entities.questions)
      it "Answer" $ mapM_ assertSchema (M.elems km1WithQ4.entities.answers)
      it "Choice" $ mapM_ assertSchema (M.elems km1WithQ4.entities.choices)
      it "Expert" $ mapM_ assertSchema (M.elems km1WithQ4.entities.experts)
      it "Reference" $ mapM_ assertSchema (M.elems km1WithQ4.entities.references)
      it "Integration" $ mapM_ assertSchema (M.elems km1WithQ4.entities.integrations)
      it "Tag" $ mapM_ assertSchema (M.elems km1WithQ4.entities.tags)
      it "Metric" $ mapM_ assertSchema (M.elems km1WithQ4.entities.metrics)
      it "Phase" $ mapM_ assertSchema (M.elems km1WithQ4.entities.phases)
      it "ResourceCollection" $ mapM_ assertSchema (M.elems km1WithQ4.entities.resourceCollections)
      it "ResourcePage" $ mapM_ assertSchema (M.elems km1WithQ4.entities.resourcePages)
      it "DocumentTemplateDraftList" $ assertSchema (toDraftList wizardDocumentTemplateDraft)
      it "DocumentTemplateDraftDetail" $ assertSchema (toDraftDetail wizardDocumentTemplateDraft wizardDocumentTemplateDraftFormats wizardDocumentTemplateDraftData (Just (toSuggestion project1)) Nothing)
      it "DocumentTemplateDraftChangeDTO" $ assertSchema (wizardDocumentTemplateDraftChangeDTO {formats = fmap toFormatDTO wizardDocumentTemplateDraftFormats} :: DocumentTemplateDraftChangeDTO)
      it "DocumentTemplateSimpleDTO" $ assertSchema wizardDocumentTemplateSimpleDTO
      it "DocumentTemplateDetailDTO" $ assertSchema wizardDocumentTemplateDetailDTO
      it "DocumentTemplateSimple" $ assertSchema wizardDocumentTemplateSimple
      it "McpDocumentPreviewDTO" $ mapM_ assertSchema [toMetadataResult tempDocDone, toTextResult tempDocDone doc1Content, toErrorResult True tempDocError]
      it "Reply" $ mapM_ assertSchema (M.elems fReplies)
      it "ProjectCreateDTO" $ assertSchema project1Create
      it "ProjectSettingsChangeDTO" $ assertSchema project1SettingsChange
      it "McpProjectDTO" $ assertSchema (toMcpProjectDTO project1)
      it "SettingsUsers" $ assertSchema settingsUsers
      it "SettingsRoles" $ assertSchema settingsRoles
      it "SettingsLoginScreen" $ assertSchema settingsLoginScreen
      it "SettingsFeatures" $ assertSchema settingsFeatures
      it "SettingsDashboardAndMenu" $ assertSchema settingsDashboardAndMenu
      it "SettingsDTO" $ assertSchema (SettingsDTO settingsProjects (Just True) (Just False))
      it "Workspace" $ assertSchema defaultWorkspace
      it "WorkspaceChangeDTO" $ assertSchema workspaceCreate
      it "McpWorkspaceMembersDTO" $ assertSchema (McpWorkspaceMembersDTO [McpWorkspaceMemberDTO userAlbert.uuid (Just defaultWorkspaceAdminRole.uuid)] [userNikola.uuid])
      it "UserGroupList" $ assertSchema (UserGroupMapper.toList bioGroup True False)
      it "UserGroupDetailDTO" $ assertSchema (UserGroupMapper.toDetailDTO bioGroup [])
      it "UserDTO" $ assertSchema userAlbertDto
      it "KnowledgeModelEventData" $ mapM_ (assertSchema . (.content)) (knowledgeModelEventExamples ++ amsterdamEvents ++ germanyEvents)

assertSchema :: forall a. (ToJSON a, FromJSON a, JsonSchema a) => a -> Expectation
assertSchema value = do
  let schema = jsonSchema (Proxy :: Proxy a)
  let json = toJSON value
  validate schema json `shouldBe` Right ()
  validate schema (object []) `shouldSatisfy` isLeft
  (toJSON <$> (fromJSON json :: Result a)) `shouldBe` Success json
