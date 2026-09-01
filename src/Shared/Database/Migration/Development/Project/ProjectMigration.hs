module Shared.Database.Migration.Development.Project.ProjectMigration where

import Data.Foldable (traverse_)

import Shared.Constant.Component
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Project.ProjectCommentDAO
import Shared.Database.DAO.Project.ProjectCommentThreadDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.Project.ProjectFileDAO
import Shared.Database.DAO.Project.ProjectPermDAO
import Shared.Database.DAO.Project.ProjectVersionDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.ProjectComments
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.Context.WizardRequestContext
import Shared.S3.Project.ProjectFileS3
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Project/Project) started"
  deleteProjectFiles
  purgeBucket
  deleteProjectComments
  deleteProjectCommentThreads
  deleteProjectPerms
  deleteProjectEvents
  deleteProjects
  insertPackage germanyKmPackage
  insertProject project1
  insertProjectEvents (fEvents project1Uuid)
  traverse_ insertProjectVersion project1Versions
  insertProject project2
  insertProjectEvents (fEvents project2Uuid)
  traverse_ insertProjectVersion project2Versions
  insertProject project3
  insertProjectEvents (fEvents project3Uuid)
  traverse_ insertProjectVersion project3Versions
  insertProject differentProject
  insertProjectCommentThread cmtQ1_t1
  insertProjectComment cmtQ1_t1_1
  insertProjectComment cmtQ1_t1_2
  insertProjectCommentThread cmtQ2_t1
  insertProjectComment cmtQ2_t1_1
  logInfo _CMP_MIGRATION "(Project/Project) ended"
