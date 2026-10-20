module Shared.Model.DocumentTemplate.DocumentTemplateDraftPublication where

import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateFileList

data DocumentTemplateDraftPublication = DocumentTemplateDraftPublication
  { draft :: DocumentTemplate
  , latestRelease :: Maybe DocumentTemplate
  , formats :: [DocumentTemplateFormat]
  , files :: [DocumentTemplateFileList]
  , assets :: [DocumentTemplateAsset]
  }
