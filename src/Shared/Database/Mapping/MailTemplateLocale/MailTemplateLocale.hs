module Shared.Database.Mapping.MailTemplateLocale.MailTemplateLocale where

import Database.PostgreSQL.Simple

import Shared.Model.MailTemplateLocale.MailTemplateLocale

instance ToRow MailTemplateLocale

instance FromRow MailTemplateLocale
