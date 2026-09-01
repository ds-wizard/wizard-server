module Shared.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveDTO
import Shared.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFolders
import Shared.Util.Swagger

instance ToSchema DocumentTemplateFolderMoveDTO where
  declareNamedSchema = toSwagger folderMoveDto
