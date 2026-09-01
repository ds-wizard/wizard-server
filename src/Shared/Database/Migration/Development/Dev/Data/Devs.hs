module Shared.Database.Migration.Development.Dev.Data.Devs where

import Shared.Api.Resource.Dev.DevExecutionDTO
import Shared.Api.Resource.Dev.DevExecutionResultDTO
import Shared.Api.Resource.Dev.DevOperationDTO
import Shared.Api.Resource.Dev.DevSectionDTO
import Shared.Model.Dev.Dev

sectionDto :: DevSectionDTO
sectionDto =
  DevSectionDTO
    { name = "Cache"
    , description = Nothing
    , operations = [operationDto]
    }

operationDto :: DevOperationDTO
operationDto =
  DevOperationDTO
    { name = "purgeCache"
    , description = Nothing
    , parameters = [operationParam1]
    }

operationParam1 :: DevOperationParameter
operationParam1 =
  DevOperationParameter
    { name = "firstParam"
    , aType = StringDevOperationParameterType
    }

execution1 :: DevExecutionDTO
execution1 =
  DevExecutionDTO
    { sectionName = sectionDto.name
    , operationName = operationDto.name
    , parameters = []
    }

execution1Result :: AdminExecutionResultDTO
execution1Result = AdminExecutionResultDTO {output = "My output"}
