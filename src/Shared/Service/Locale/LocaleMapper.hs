module Shared.Service.Locale.LocaleMapper where

import Shared.Model.Locale.Locale
import Shared.Model.Locale.LocaleSimple
import Shared.Model.Locale.LocaleSuggestion

toLocaleSuggestion :: Locale -> LocaleSuggestion
toLocaleSuggestion locale =
  LocaleSuggestion
    { uuid = locale.uuid
    , name = locale.name
    , description = locale.description
    , code = locale.code
    , id = locale.id
    , version = locale.version
    , defaultLocale = locale.defaultLocale
    }

toSimple :: Locale -> LocaleSimple
toSimple Locale {..} = LocaleSimple {..}
