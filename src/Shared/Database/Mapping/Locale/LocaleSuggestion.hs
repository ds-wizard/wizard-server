module Shared.Database.Mapping.Locale.LocaleSuggestion where

import Database.PostgreSQL.Simple

import Shared.Model.Locale.LocaleSuggestion

instance FromRow LocaleSuggestion
