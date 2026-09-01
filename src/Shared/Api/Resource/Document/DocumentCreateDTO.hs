module Shared.Api.Resource.Document.DocumentCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data DocumentCreateDTO = DocumentCreateDTO
  { name :: String
  , projectUuid :: U.UUID
  , projectEventUuid :: Maybe U.UUID
  , documentTemplateUuid :: U.UUID
  , formatUuid :: U.UUID
  , language :: Maybe String
  }
  deriving (Show, Eq, Generic)
