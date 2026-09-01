module Shared.Service.DocumentTemplate.WizardDocumentTemplateUtil where

import qualified Data.List as L

import Shared.Constant.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateList
import Shared.Model.DocumentTemplate.DocumentTemplateState
import Shared.Model.Registry.RegistryTemplate
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil

computeDocumentTemplateState :: [RegistryTemplate] -> DocumentTemplate -> DocumentTemplateState
computeDocumentTemplateState tmlsFromRegistry tml =
  if not (isDocumentTemplateSupported tml.metamodelVersion)
    then UnsupportedMetamodelVersionDocumentTemplateState
    else DefaultDocumentTemplateState

computeDocumentTemplateState' :: DocumentTemplateList -> DocumentTemplateState
computeDocumentTemplateState' tml
  | not (isDocumentTemplateSupported tml.metamodelVersion) = UnsupportedMetamodelVersionDocumentTemplateState
  | otherwise = DefaultDocumentTemplateState

selectDocumentTemplateByOrgIdAndTmlId tml =
  L.find (\t -> t.organizationId == tml.organizationId && t.templateId == tml.templateId)

selectOrganizationByOrgId tml = L.find (\org -> org.organizationId == tml.organizationId)

isDocumentTemplateInPhase (Just phase) tml = tml.phase == phase
isDocumentTemplateInPhase _ _ = True

filterDocumentTemplates mCoordinate tmls =
  case mCoordinate of
    Just coordinate -> filter (filterDocumentTemplate coordinate) tmls
    Nothing -> tmls
  where
    filterDocumentTemplate coordinate template = fitsIntoKMSpecs coordinate template.allowedPackages
