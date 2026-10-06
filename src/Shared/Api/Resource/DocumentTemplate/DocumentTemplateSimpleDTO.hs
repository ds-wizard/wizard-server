module Shared.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateState

data DocumentTemplateSimpleDTO = DocumentTemplateSimpleDTO
  { uuid :: U.UUID
  , name :: String
  , id :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , remoteLatestVersion :: Maybe String
  , description :: String
  , nonEditable :: Bool
  , language :: String
  , potFileReady :: Bool
  , state :: DocumentTemplateState
  , createdAt :: UTCTime
  , workspaceUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)
