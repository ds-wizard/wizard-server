module Shared.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderDeleteSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderDeleteDTO
import Shared.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderDeleteJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFolders
import Shared.Util.Swagger

instance ToSchema DocumentTemplateFolderDeleteDTO where
  declareNamedSchema = toSwagger folderDeleteDto
