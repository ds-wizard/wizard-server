module Shared.Service.DocumentTemplate.Locale.DocumentTemplateLocaleMapper where

import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocale
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocaleList

toList :: DocumentTemplateLocale -> DocumentTemplateLocaleList
toList locale =
  DocumentTemplateLocaleList
    { uuid = locale.uuid
    , name = locale.name
    , code = locale.code
    }
