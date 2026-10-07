module Shared.Service.Registry.Push.RegistryPushService where

import qualified Data.UUID as U

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Integration.Http.Registry.Runner
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import qualified Shared.Service.DocumentTemplate.Bundle.DocumentTemplateBundleService as DocumentTemplateBundleService
import qualified Shared.Service.KnowledgeModel.Bundle.KnowledgeModelBundleService as KnowledgeModelBundleService
import Shared.Util.Logger
import Shared.Util.Reference

pushKnowledgeModelBundle :: WizardRequestContextC s m => String -> Maybe U.UUID -> m ()
pushKnowledgeModelBundle pkgId workspaceUuid = do
  logInfoI _CMP_SERVICE (f' "Pushing knowledge model bundle with the id ('%s') to registry" [pkgId])
  coordinate <- parseCoordinate pkgId
  pkg <- findPackageByCoordinate coordinate workspaceUuid
  bundle <- KnowledgeModelBundleService.exportBundle pkg.uuid
  uploadKnowledgeModelBundle bundle
  logInfoI _CMP_SERVICE (f' "Pushing knowledge model bundle with the id ('%s') successfully completed" [pkgId])

pushDocumentTemplateBundle :: WizardRequestContextC s m => String -> Maybe U.UUID -> m ()
pushDocumentTemplateBundle dtId workspaceUuid = do
  logInfoI _CMP_SERVICE (f' "Pushing document template bundle with the id ('%s') to registry" [dtId])
  coordinate <- parseCoordinate dtId
  dt <- findDocumentTemplateByCoordinate coordinate workspaceUuid
  (coordinate, bundle) <- DocumentTemplateBundleService.exportBundle dt.uuid
  uploadDocumentTemplateBundle bundle
  logInfoI _CMP_SERVICE (f' "Pushing document template bundle with the id ('%s') successfully completed" [dtId])
