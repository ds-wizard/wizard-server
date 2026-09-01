module Shared.Service.Report.ReportService where

import qualified Data.UUID as U

import Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Shared.Api.Resource.Project.Detail.ProjectDetailReportDTO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Service.KnowledgeModel.KnowledgeModelService
import Shared.Service.KnowledgeModel.Locale.KnowledgeModelLocaleService (findLocaleJson)
import Shared.Service.Project.ProjectService
import Shared.Service.Report.ReportGenerator
import Shared.Service.Report.ReportMapper

getReportByProjectUuid :: WizardRequestContextC s m => U.UUID -> m ProjectDetailReportDTO
getReportByProjectUuid projectUuid = do
  projectDto <- getProjectDetailQuestionnaireByUuid projectUuid
  generateProjectReport projectDto

generateProjectReport :: WizardRequestContextC s m => ProjectDetailQuestionnaireDTO -> m ProjectDetailReportDTO
generateProjectReport projectDto = do
  knowledgeModel <- compileKnowledgeModel [] (Just projectDto.knowledgeModelPackage.uuid) projectDto.selectedQuestionTagUuids
  report <- generateReport projectDto.phaseUuid knowledgeModel projectDto.replies
  mLocale <- findLocaleJson projectDto.knowledgeModelPackage.uuid projectDto.language
  return $ toDTO projectDto report mLocale
