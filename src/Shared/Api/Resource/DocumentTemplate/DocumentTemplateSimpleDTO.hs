module Shared.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import RegistryPublic.Model.Organization.OrganizationSimple
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateState

data DocumentTemplateSimpleDTO = DocumentTemplateSimpleDTO
  { uuid :: U.UUID
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , remoteLatestVersion :: Maybe String
  , description :: String
  , nonEditable :: Bool
  , language :: String
  , potFileReady :: Bool
  , state :: DocumentTemplateState
  , organization :: Maybe OrganizationSimple
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
