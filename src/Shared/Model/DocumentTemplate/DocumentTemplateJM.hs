module Shared.Model.DocumentTemplate.DocumentTemplateJM where

import Data.Aeson

import Shared.Api.Resource.Common.SemVer2TupleJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePatternJM ()
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance ToJSON DocumentTemplatePhase

instance FromJSON DocumentTemplatePhase

instance FromJSON DocumentTemplate where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplate where
  toJSON = genericToJSON jsonOptions

instance FromJSON DocumentTemplateFormat where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFormat where
  toJSON = genericToJSON jsonOptions

instance FromJSON DocumentTemplateFormatStep where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFormatStep where
  toJSON = genericToJSON jsonOptions

instance FromJSON DocumentTemplateFile where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFile where
  toJSON = genericToJSON jsonOptions

instance FromJSON DocumentTemplateAsset where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateAsset where
  toJSON = genericToJSON jsonOptions

instance JsonSchema DocumentTemplatePhase where
  jsonSchema = genericJsonSchema defaultOptions

instance JsonSchema DocumentTemplateFormat where
  jsonSchema = genericJsonSchema jsonOptions

instance JsonSchema DocumentTemplateFormatStep where
  jsonSchema = genericJsonSchema jsonOptions
