module Shared.Service.Project.File.ProjectFileValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Model.Project.File.ProjectFile
import Shared.Model.Project.Project
import Shared.Service.KnowledgeModel.KnowledgeModelService
import Shared.Service.Tenant.Limit.WizardLimitService

validateProjectFile :: WizardRequestContextC s m => Project -> U.UUID -> ProjectFile -> m ()
validateProjectFile project questionUuid projectFile = do
  checkStorageSize projectFile.fileSize
  km <- compileKnowledgeModel [] (Just project.knowledgeModelPackageUuid) project.selectedQuestionTagUuids
  case M.lookup questionUuid (getQuestionsM km) of
    Just (FileQuestion' question) ->
      case question.maxSize of
        (Just maxFileSize) ->
          when
            (maxFileSize < fromIntegral projectFile.fileSize)
            (throwError . UserError $ _ERROR_VALIDATION__PROJECT_FILE_SIZE_EXCEEDS_LIMIT projectFile.fileSize maxFileSize)
        Nothing -> return ()
    _ -> throwError . UserError $ _ERROR_VALIDATION__PROJECT_FILE_QUESTION_ABSENCE_OR_WRONG_TYPE
