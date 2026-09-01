module Shared.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleListSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleListJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Locale.KnowledgeModelLocales
import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList
import Shared.Util.Swagger

instance ToSchema KnowledgeModelLocaleList where
  declareNamedSchema = toSwagger czechGlobalKmLocaleList
