module Shared.Service.Registry.Push.RegistryPushService where

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Integration.Http.Registry.Runner
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Locale.Locale
import qualified Shared.Service.DocumentTemplate.Bundle.DocumentTemplateBundleService as DocumentTemplateBundleService
import qualified Shared.Service.KnowledgeModel.Bundle.KnowledgeModelBundleService as KnowledgeModelBundleService
import qualified Shared.Service.Locale.Bundle.LocaleBundleService as LocaleBundleService
import Shared.Util.Coordinate
import Shared.Util.Logger

pushKnowledgeModelBundle :: WizardRequestContextC s m => String -> m ()
pushKnowledgeModelBundle pkgId = do
  logInfoI _CMP_SERVICE (f' "Pushing knowledge model bundle with the id ('%s') to registry" [pkgId])
  coordinate <- parseCoordinate pkgId
  pkg <- findPackageByCoordinate coordinate
  bundle <- KnowledgeModelBundleService.exportBundle pkg.uuid
  uploadKnowledgeModelBundle bundle
  logInfoI _CMP_SERVICE (f' "Pushing knowledge model bundle with the id ('%s') successfully completed" [pkgId])

pushDocumentTemplateBundle :: WizardRequestContextC s m => String -> m ()
pushDocumentTemplateBundle dtId = do
  logInfoI _CMP_SERVICE (f' "Pushing document template bundle with the id ('%s') to registry" [dtId])
  coordinate <- parseCoordinate dtId
  dt <- findDocumentTemplateByCoordinate coordinate
  (coordinate, bundle) <- DocumentTemplateBundleService.exportBundle dt.uuid
  uploadDocumentTemplateBundle bundle
  logInfoI _CMP_SERVICE (f' "Pushing document template bundle with the id ('%s') successfully completed" [dtId])

pushLocaleBundle :: WizardRequestContextC s m => String -> m ()
pushLocaleBundle lId = do
  logInfoI _CMP_SERVICE (f' "Pushing locale bundle with the id ('%s') to registry" [lId])
  coordinate <- parseCoordinate lId
  locale <- findLocaleByCoordinate coordinate
  (coordinate, bundle) <- LocaleBundleService.exportBundle locale.uuid
  uploadLocaleBundle bundle
  logInfoI _CMP_SERVICE (f' "Pushing locale bundle with the id ('%s') successfully completed" [lId])
