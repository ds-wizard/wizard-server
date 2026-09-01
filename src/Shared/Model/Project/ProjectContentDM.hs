module Shared.Model.Project.ProjectContentDM where

import qualified Data.Map.Strict as M

import Shared.Model.Project.ProjectContent

defaultProjectContent :: ProjectContent
defaultProjectContent =
  ProjectContent
    { phaseUuid = Nothing
    , replies = M.empty
    , labels = M.empty
    }
