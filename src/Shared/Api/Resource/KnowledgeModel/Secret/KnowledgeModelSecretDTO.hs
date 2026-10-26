module Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModelSecretDTO = KnowledgeModelSecretDTO
  { uuid :: U.UUID
  , name :: String
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , workspaceUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)
