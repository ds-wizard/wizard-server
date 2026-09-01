module Shared.Service.Dev.DevOperationMapper where

import Shared.Api.Resource.Dev.DevOperationDTO
import Shared.Api.Resource.Dev.DevSectionDTO
import Shared.Model.Dev.Dev

toDevSectionDTO :: DevSection m -> DevSectionDTO
toDevSectionDTO section =
  DevSectionDTO
    { name = section.name
    , description = section.description
    , operations = fmap toDevOperationDTO section.operations
    }

toDevOperationDTO :: DevOperation m -> DevOperationDTO
toDevOperationDTO operation =
  DevOperationDTO
    { name = operation.name
    , description = operation.description
    , parameters = operation.parameters
    }
