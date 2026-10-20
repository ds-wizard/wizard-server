module Shared.Service.Registry.RegistryCatalogService where

import Data.Char (toLower)
import Data.List (isInfixOf, sortOn)

import Shared.Api.Resource.Registry.RegistryCatalogItemDTO
import Shared.Database.DAO.Registry.RegistryKnowledgeModelPackageDAO
import Shared.Database.DAO.Registry.RegistryTemplateDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Registry.RegistryPackage
import Shared.Model.Registry.RegistryTemplate
import Shared.Model.Settings.Settings
import Shared.Service.Common
import Shared.Service.Registry.RegistryMapper
import Shared.Service.Settings.OrganizationSettingsService

searchRegistryCatalog :: WizardRequestContextC s m => Maybe String -> m [RegistryCatalogItemDTO]
searchRegistryCatalog mQuery = do
  checkIfTenantFeatureIsEnabled "Registry" getCurrentSettingsRegistry (.enabled)
  packages <- findRegistryPackages
  templates <- findRegistryTemplates
  let items = fmap toPackageCatalogItem (sortOn (\p -> p.remoteName) packages) ++ fmap toTemplateCatalogItem (sortOn (\t -> t.remoteName) templates)
  return $ filter (matchesQuery mQuery) items

matchesQuery :: Maybe String -> RegistryCatalogItemDTO -> Bool
matchesQuery Nothing _ = True
matchesQuery (Just query) item = any ((fmap toLower query `isInfixOf`) . fmap toLower) [item.id, item.name, item.description]
