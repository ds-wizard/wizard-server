module Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.Registry.RegistryOrganizationJM ()
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSimple
import Shared.Util.Aeson

instance FromJSON KnowledgeModelPackageSimpleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackageSimpleDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON KnowledgeModelPackageSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackageSimple where
  toJSON = genericToJSON jsonOptions
