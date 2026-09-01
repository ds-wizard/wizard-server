module Shared.Service.Statistics.StatisticsService where

import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Statistics.InstanceStatistics

getInstanceStatistics :: WizardRequestContextC s m => m InstanceStatistics
getInstanceStatistics = do
  uCount <- countUsers
  pCount <- countPackagesGroupedByOrganizationIdAndKmId
  qCount <- countProjects
  bCount <- countKnowledgeModelEditors
  docCount <- countDocuments
  tmlCount <- countDocumentTemplatesGroupedByOrganizationIdAndKmId
  return
    InstanceStatistics
      { userCount = uCount
      , pkgCount = pCount
      , prjCount = qCount
      , knowledgeModelEditorCount = bCount
      , docCount = docCount
      , tmlCount = tmlCount
      }
