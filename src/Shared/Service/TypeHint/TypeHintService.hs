module Shared.Service.TypeHint.TypeHintService where

import Control.Monad.Except (throwError)
import Data.Map.Strict as M

import Shared.Api.Resource.TypeHint.TypeHintRequestDTO
import Shared.Api.Resource.TypeHint.TypeHintTestRequestDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelSecretDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.WizardCommon
import Shared.Integration.Http.TypeHint.Runner
import Shared.Integration.Resource.TypeHint.TypeHintIDTO
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Model.KnowledgeModel.KnowledgeModelSecret
import Shared.Model.Project.Project
import Shared.Service.Config.Integration.IntegrationConfigService
import Shared.Service.KnowledgeModel.Editor.EditorMapper
import Shared.Service.KnowledgeModel.KnowledgeModelService
import Shared.Service.Project.ProjectAcl
import Shared.Util.Logger

getTypeHints :: WizardRequestContextC s m => TypeHintRequestDTO -> m [TypeHintIDTO]
getTypeHints (KnowledgeModelEditorIntegrationTypeHintRequest' reqDto) =
  runInTransaction $ do
    checkPermission _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
    kmEditor <- findKnowledgeModelEditorByUuid reqDto.knowledgeModelEditorUuid
    kmEditorEvents <- findKnowledgeModelEventsByEditorUuid reqDto.knowledgeModelEditorUuid
    let kmEvents = fmap toKnowledgeModelEvent kmEditorEvents
    km <- compileKnowledgeModel kmEvents kmEditor.previousPackageUuid []
    integration' <- getIntegration km reqDto.integrationUuid
    case integration' of
      ApiIntegration' integration -> runApiIntegrationTypeHints integration integration.testVariables integration.testQ
      _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION
getTypeHints (KnowledgeModelEditorQuestionTypeHintRequest' reqDto) =
  runInTransaction $ do
    checkPermission _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
    kmEditor <- findKnowledgeModelEditorByUuid reqDto.knowledgeModelEditorUuid
    kmEditorEvents <- findKnowledgeModelEventsByEditorUuid reqDto.knowledgeModelEditorUuid
    let kmEvents = fmap toKnowledgeModelEvent kmEditorEvents
    km <- compileKnowledgeModel kmEvents kmEditor.previousPackageUuid []
    question <- getQuestion km reqDto.questionUuid
    integration' <- getIntegration km question.integrationUuid
    case integration' of
      ApiIntegration' integration -> runApiIntegrationTypeHints integration question.variables reqDto.q
      _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION
getTypeHints (ProjectTypeHintRequest' reqDto) =
  runInTransaction $ do
    project <- findProjectByUuid reqDto.projectUuid
    checkEditPermissionToProject project.visibility project.sharing project.permissions
    km <- compileKnowledgeModel [] (Just project.knowledgeModelPackageUuid) []
    question <- getQuestion km reqDto.questionUuid
    integration' <- getIntegration km question.integrationUuid
    case integration' of
      ApiIntegration' integration -> runApiIntegrationTypeHints integration question.variables reqDto.q
      _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION

runApiIntegrationTypeHints :: WizardRequestContextC s m => ApiIntegration -> M.Map String String -> String -> m [TypeHintIDTO]
runApiIntegrationTypeHints integration variables q =
  runInTransaction $ do
    secrets <- prepareSecrets
    eiDtos <- retrieveTypeHints integration variables secrets q
    case eiDtos of
      Right iDtos -> return iDtos
      Left error -> do
        logWarnI _CMP_SERVICE error
        throwError . UserError $ _ERROR_SERVICE_TYPEHINT__INTEGRATION_RETURNS_ERROR

testTypeHints :: WizardRequestContextC s m => TypeHintTestRequestDTO -> m TypeHintExchange
testTypeHints reqDto =
  runInTransaction $ do
    checkPermission _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
    kmEditor <- findKnowledgeModelEditorByUuid reqDto.knowledgeModelEditorUuid
    kmEditorEvents <- findKnowledgeModelEventsByEditorUuid reqDto.knowledgeModelEditorUuid
    let kmEvents = fmap toKnowledgeModelEvent kmEditorEvents
    km <- compileKnowledgeModel kmEvents kmEditor.previousPackageUuid []
    integration' <- getIntegration km reqDto.integrationUuid
    case integration' of
      ApiIntegration' integration -> do
        secrets <- prepareSecrets
        testRetrieveTypeHints integration reqDto.variables secrets reqDto.q
      _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION

-- --------------------------------
-- PRIVATE
-- --------------------------------
getQuestion km questionUuid =
  case M.lookup questionUuid (getQuestionsM km) of
    Just (IntegrationQuestion' question) -> return question
    Just _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_QUESTION
    Nothing -> throwError . UserError $ _ERROR_VALIDATION__QUESTION_ABSENCE

getIntegration km integrationUuid =
  case M.lookup integrationUuid (getIntegrationsM km) of
    Just integration -> return integration
    Nothing -> throwError . UserError $ _ERROR_VALIDATION__INTEGRATION_ABSENCE

prepareSecrets :: WizardRequestContextC s m => m (M.Map String String)
prepareSecrets = do
  kmSecrets <- fmap (M.fromList . fmap (\s -> (s.name, s.value))) findKnowledgeModelSecrets
  fileSecrets <- getFileIntegrationConfig "secrets"
  return $ M.union kmSecrets fileSecrets
