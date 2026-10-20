module Shared.Service.TypeHint.TypeHintService where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeDTO

import Shared.Api.Resource.TypeHint.TypeHintRequestDTO
import Shared.Api.Resource.TypeHint.TypeHintTestRequestDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelSecretDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.WizardCommon
import Shared.Integration.Http.TypeHint.RequestMapper (rendersSecrets)
import Shared.Integration.Http.TypeHint.ResponseMapper (maskSecretsInResponse)
import Shared.Integration.Http.TypeHint.Runner
import Shared.Integration.Resource.TypeHint.TypeHintIDTO
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Error.FieldError
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Model.KnowledgeModel.KnowledgeModelSecret
import Shared.Model.Project.Project
import Shared.Service.Config.Integration.IntegrationConfigService
import Shared.Service.KnowledgeModel.Editor.EditorAcl
import Shared.Service.KnowledgeModel.Editor.EditorUtil (findEditorKnowledgeModel)
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventNames (resolveUuid)
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventService (compileEditorWithPendingEvents)
import Shared.Service.Project.ProjectAcl
import Shared.Service.Project.ProjectUtil (compileProjectKnowledgeModel)
import Shared.Util.Logger

getTypeHints :: WizardRequestContextC s m => TypeHintRequestDTO -> m [TypeHintIDTO]
getTypeHints (KnowledgeModelEditorIntegrationTypeHintRequest' reqDto) =
  runInTransaction $ do
    checkPermissionToEditor reqDto.knowledgeModelEditorUuid
    (kmEditor, km) <- findEditorKnowledgeModel reqDto.knowledgeModelEditorUuid []
    integration' <- getIntegration km reqDto.integrationUuid
    case integration' of
      ApiIntegration' integration -> runApiIntegrationTypeHints (Just kmEditor.workspaceUuid) integration integration.testVariables integration.testQ
      _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION
getTypeHints (KnowledgeModelEditorQuestionTypeHintRequest' reqDto) =
  runInTransaction $ do
    checkPermissionToEditor reqDto.knowledgeModelEditorUuid
    (kmEditor, km) <- findEditorKnowledgeModel reqDto.knowledgeModelEditorUuid []
    question <- getQuestion km reqDto.questionUuid
    integration' <- getIntegration km question.integrationUuid
    case integration' of
      ApiIntegration' integration -> runApiIntegrationTypeHints (Just kmEditor.workspaceUuid) integration question.variables reqDto.q
      _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION
getTypeHints (ProjectTypeHintRequest' reqDto) =
  runInTransaction $ do
    project <- findProjectByUuid reqDto.projectUuid
    checkEditPermissionToProject project.workspaceUuid project.visibility project.sharing project.permissions
    km <- compileProjectKnowledgeModel project.knowledgeModelPackageUuid []
    question <- getQuestion km reqDto.questionUuid
    integration' <- getIntegration km question.integrationUuid
    case integration' of
      ApiIntegration' integration -> runApiIntegrationTypeHints (Just project.workspaceUuid) integration question.variables reqDto.q
      _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION

runApiIntegrationTypeHints :: WizardRequestContextC s m => Maybe U.UUID -> ApiIntegration -> M.Map String String -> String -> m [TypeHintIDTO]
runApiIntegrationTypeHints mWorkspaceUuid integration variables q =
  runInTransaction $ do
    secrets <- prepareSecrets mWorkspaceUuid
    eiDtos <- retrieveTypeHints integration variables secrets q
    case eiDtos of
      Right iDtos -> return iDtos
      Left error -> do
        logWarnI _CMP_SERVICE error
        throwError . UserError $ _ERROR_SERVICE_TYPEHINT__INTEGRATION_RETURNS_ERROR

testTypeHints :: WizardRequestContextC s m => TypeHintTestRequestDTO -> m TypeHintExchange
testTypeHints reqDto =
  runInTransaction $ do
    checkPermissionToEditor reqDto.knowledgeModelEditorUuid
    (kmEditor, km) <- findEditorKnowledgeModel reqDto.knowledgeModelEditorUuid []
    integration' <- getIntegration km reqDto.integrationUuid
    case integration' of
      ApiIntegration' integration -> do
        secrets <- prepareSecrets (Just kmEditor.workspaceUuid)
        testRetrieveTypeHints integration reqDto.variables secrets reqDto.q
      _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION

testTypeHintsWithPendingEvents
  :: WizardRequestContextC s m
  => U.UUID
  -> [KnowledgeModelEditorEventChangeDTO]
  -> String
  -> Maybe (M.Map String String)
  -> Maybe String
  -> m TypeHintExchange
testTypeHintsWithPendingEvents editorUuid pendingEvents integrationName mVariables mQ =
  runInTransaction $ do
    (kmEditor, storedKm, km, names) <- compileEditorWithPendingEvents editorUuid pendingEvents
    integrationUuid <- either (throwError . fieldValidationError []) return (resolveUuid names "integration_uuid" integrationName)
    integration' <- getIntegration km integrationUuid
    case integration' of
      ApiIntegration' integration -> do
        let variables = fromMaybe integration.testVariables mVariables
        let q = fromMaybe integration.testQ mQ
        secrets <- prepareStoredIntegrationSecrets kmEditor.workspaceUuid storedKm integration variables q
        maskSecretsInResponse secrets <$> testRetrieveTypeHints integration variables secrets q
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

prepareStoredIntegrationSecrets :: WizardRequestContextC s m => U.UUID -> KnowledgeModel -> ApiIntegration -> M.Map String String -> String -> m (M.Map String String)
prepareStoredIntegrationSecrets workspaceUuid storedKm integration variables q
  | M.lookup integration.uuid (getIntegrationsM storedKm) /= Just (ApiIntegration' integration) = return M.empty
  | otherwise = do
      secrets <- prepareSecrets (Just workspaceUuid)
      usesSecrets <- liftIO $ rendersSecrets integration variables secrets q
      when usesSecrets (checkPermission _MCP_SENSITIVE_ROLE_PERMISSION)
      return secrets

prepareSecrets :: WizardRequestContextC s m => Maybe U.UUID -> m (M.Map String String)
prepareSecrets mWorkspaceUuid = do
  kmSecrets <- fmap (M.fromList . fmap (\s -> (s.name, s.value))) (findKnowledgeModelSecretsForWorkspace mWorkspaceUuid)
  fileSecrets <- getFileIntegrationConfig "secrets"
  return $ M.union kmSecrets fileSecrets
