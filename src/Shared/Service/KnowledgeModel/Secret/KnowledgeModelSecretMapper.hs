module Shared.Service.KnowledgeModel.Secret.KnowledgeModelSecretMapper where

import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U
import Prelude hiding (id)

import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeDTO
import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretDTO
import Shared.Model.KnowledgeModel.KnowledgeModelSecret

toDTO :: KnowledgeModelSecret -> KnowledgeModelSecretDTO
toDTO secret =
  KnowledgeModelSecretDTO
    { uuid = secret.uuid
    , name = secret.name
    , tenantUuid = secret.tenantUuid
    , createdAt = secret.createdAt
    , updatedAt = secret.updatedAt
    , workspaceUuid = secret.workspaceUuid
    }

fromCreateDTO :: KnowledgeModelSecretChangeDTO -> String -> U.UUID -> U.UUID -> Maybe U.UUID -> UTCTime -> KnowledgeModelSecret
fromCreateDTO dto value uuid tenantUuid workspaceUuid now =
  KnowledgeModelSecret
    { uuid = uuid
    , name = dto.name
    , value = value
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    , workspaceUuid = workspaceUuid
    }

toChangeDTO :: KnowledgeModelSecret -> KnowledgeModelSecretChangeDTO
toChangeDTO secret = KnowledgeModelSecretChangeDTO secret.name (Just secret.value)

fromChangeDTO :: KnowledgeModelSecret -> KnowledgeModelSecretChangeDTO -> UTCTime -> KnowledgeModelSecret
fromChangeDTO kmSecret dto now =
  KnowledgeModelSecret
    { uuid = kmSecret.uuid
    , name = dto.name
    , value = fromMaybe kmSecret.value dto.value
    , tenantUuid = kmSecret.tenantUuid
    , createdAt = kmSecret.createdAt
    , updatedAt = now
    , workspaceUuid = kmSecret.workspaceUuid
    }
