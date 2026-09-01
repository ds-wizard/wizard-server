module WizardServer.Database.Mapping.Locale.LocaleSimple where

import Database.PostgreSQL.Simple

import Shared.Model.Locale.LocaleSimple

instance FromRow LocaleSimple
