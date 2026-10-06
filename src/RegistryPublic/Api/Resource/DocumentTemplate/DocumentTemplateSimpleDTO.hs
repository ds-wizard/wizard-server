module RegistryPublic.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data DocumentTemplateSimpleDTO = DocumentTemplateSimpleDTO
  { uuid :: U.UUID
  , name :: String
  , id :: String
  , version :: String
  , description :: String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
