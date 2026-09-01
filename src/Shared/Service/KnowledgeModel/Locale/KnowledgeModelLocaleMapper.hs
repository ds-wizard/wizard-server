module Shared.Service.KnowledgeModel.Locale.KnowledgeModelLocaleMapper where

import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocale
import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList

toList :: KnowledgeModelLocale -> KnowledgeModelLocaleList
toList locale =
  KnowledgeModelLocaleList
    { uuid = locale.uuid
    , name = locale.name
    , code = locale.code
    }
