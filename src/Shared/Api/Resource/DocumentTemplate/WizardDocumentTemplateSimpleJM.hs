module Shared.Api.Resource.DocumentTemplate.WizardDocumentTemplateSimpleJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateStateJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON DocumentTemplateSimpleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateSimpleDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema DocumentTemplateSimpleDTO where
  jsonSchema = genericJsonSchema jsonOptions
