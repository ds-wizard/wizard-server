module Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSimple
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON KnowledgeModelPackageSimpleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackageSimpleDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON KnowledgeModelPackageSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackageSimple where
  toJSON = genericToJSON jsonOptions

instance JsonSchema KnowledgeModelPackageSimpleDTO where
  jsonSchema = genericJsonSchema jsonOptions

instance JsonSchema KnowledgeModelPackageSimple where
  jsonSchema = genericJsonSchema jsonOptions
