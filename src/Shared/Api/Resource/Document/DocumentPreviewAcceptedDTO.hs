module Shared.Api.Resource.Document.DocumentPreviewAcceptedDTO where

import qualified Data.UUID as U
import GHC.Generics

data DocumentPreviewAcceptedDTO = DocumentPreviewAcceptedDTO
  { documentUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)
