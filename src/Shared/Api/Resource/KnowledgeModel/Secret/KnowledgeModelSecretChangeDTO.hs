module Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeDTO where

import GHC.Generics

data KnowledgeModelSecretChangeDTO = KnowledgeModelSecretChangeDTO
  { name :: String
  , value :: Maybe String
  }
  deriving (Show, Eq, Generic)
