module WizardServer.Database.Mapping.User.UserGroupSuggestion where

import Database.PostgreSQL.Simple

import WizardServer.Model.User.UserGroupSuggestion

instance FromRow UserGroupSuggestion
