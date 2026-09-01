module WizardServer.Database.Mapping.Locale.LocaleList where

import Database.PostgreSQL.Simple

import WizardServer.Model.Locale.LocaleList

instance FromRow LocaleList
