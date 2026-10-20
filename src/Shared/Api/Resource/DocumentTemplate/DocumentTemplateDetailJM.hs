module Shared.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateStateJM ()
import Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleListJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Api.Resource.Version.VersionJM ()
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON DocumentTemplateDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDetailDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema DocumentTemplateDetailDTO where
  jsonSchema = genericJsonSchema jsonOptions
