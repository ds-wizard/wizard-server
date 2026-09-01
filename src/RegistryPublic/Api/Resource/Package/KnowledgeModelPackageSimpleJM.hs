module RegistryPublic.Api.Resource.Package.KnowledgeModelPackageSimpleJM where

import Data.Aeson

import RegistryPublic.Api.Resource.Organization.OrganizationSimpleJM ()
import RegistryPublic.Api.Resource.Package.KnowledgeModelPackageSimpleDTO
import Shared.Util.Aeson

instance FromJSON KnowledgeModelPackageSimpleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackageSimpleDTO where
  toJSON = genericToJSON jsonOptions
