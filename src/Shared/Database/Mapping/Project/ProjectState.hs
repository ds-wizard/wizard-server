module Shared.Database.Mapping.Project.ProjectState where

import Database.PostgreSQL.Simple.FromField

import Shared.Database.Mapping.Common
import Shared.Model.Project.ProjectState

instance FromField KnowledgeModelProjectState where
  fromField = fromFieldGenericEnum

instance FromField DocumentTemplateProjectState where
  fromField = fromFieldGenericEnum
