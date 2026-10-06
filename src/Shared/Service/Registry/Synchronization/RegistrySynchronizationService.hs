module Shared.Service.Registry.Synchronization.RegistrySynchronizationService where

import Control.Monad.Reader (liftIO)
import Data.Foldable (traverse_)
import Data.Time

import Shared.Database.DAO.Registry.RegistryKnowledgeModelPackageDAO
import Shared.Database.DAO.Registry.RegistryLocaleDAO
import Shared.Database.DAO.Registry.RegistryTemplateDAO
import Shared.Integration.Http.Registry.Runner
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings
import Shared.Service.Common
import Shared.Service.Registry.RegistryMapper
import Shared.Service.Settings.OrganizationSettingsService
import Shared.Service.Statistics.StatisticsService
import Shared.Util.Logger

synchronizeData :: WizardRequestContextC s m => m ()
synchronizeData = do
  checkIfRegistryIsEnabled
  now <- liftIO getCurrentTime
  synchronizePackages now
  synchronizeTemplates now
  synchronizeLocales now

synchronizePackages :: WizardRequestContextC s m => UTCTime -> m ()
synchronizePackages now = do
  logInfoI _CMP_SERVICE "Package Synchronization started"
  iStat <- getInstanceStatistics
  packages <- retrievePackages iStat
  let registryPackages = fmap (`toRegistryPackage` now) packages
  deleteRegistryPackages
  traverse_ insertRegistryPackage registryPackages
  logInfoI _CMP_SERVICE "Package Synchronization successfully finished"

synchronizeTemplates :: WizardRequestContextC s m => UTCTime -> m ()
synchronizeTemplates now = do
  logInfoI _CMP_SERVICE "DocumentTemplate Synchronization started"
  templates <- retrieveDocumentTemplates
  let registryTemplates = fmap (`toRegistryTemplate` now) templates
  deleteRegistryTemplates
  traverse_ insertRegistryTemplate registryTemplates
  logInfoI _CMP_SERVICE "DocumentTemplate Synchronization successfully finished"

synchronizeLocales :: WizardRequestContextC s m => UTCTime -> m ()
synchronizeLocales now = do
  logInfoI _CMP_SERVICE "Locale Synchronization started"
  templates <- retrieveLocales
  let registryLocales = fmap (`toRegistryLocale` now) templates
  deleteRegistryLocales
  traverse_ insertRegistryLocale registryLocales
  logInfoI _CMP_SERVICE "Locale Synchronization successfully finished"

-- --------------------------------
-- PRIVATE
-- --------------------------------
checkIfRegistryIsEnabled :: WizardRequestContextC s m => m ()
checkIfRegistryIsEnabled = checkIfTenantFeatureIsEnabled "Registry" getCurrentSettingsRegistry (.enabled)
