module Shared.Database.Mapping.DocumentTemplate.DocumentTemplateState where

import Database.PostgreSQL.Simple.FromField

import Shared.Database.Mapping.Common
import Shared.Model.DocumentTemplate.DocumentTemplateState

instance FromField DocumentTemplateState where
  fromField = fromFieldGenericEnum
