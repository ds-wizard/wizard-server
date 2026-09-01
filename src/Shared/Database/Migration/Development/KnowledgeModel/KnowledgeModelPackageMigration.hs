module Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration where

import Data.Foldable (traverse_)
import Shared.Constant.Component
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.WizardKnowledgeModelPackages
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Package/Package) started"
  deletePackages
  insertPackage globalKmPackageEmpty
  traverse_ insertPackageEvent globalKmPackageEmptyEvents
  insertPackage globalKmPackage
  traverse_ insertPackageEvent globalKmPackageEvents
  insertPackage netherlandsKmPackage
  traverse_ insertPackageEvent netherlandsKmPackageEvents
  insertPackage netherlandsKmPackageV2
  traverse_ insertPackageEvent netherlandsKmPackageV2Events
  insertPackage differentPackage
  traverse_ insertPackageEvent differentPackageEvents
  logInfo _CMP_MIGRATION "(Package/Package) ended"
