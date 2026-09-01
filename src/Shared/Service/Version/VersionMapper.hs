module Shared.Service.Version.VersionMapper where

import qualified Data.UUID as U

import Shared.Api.Resource.Version.VersionDTO

toVersionDTO :: (U.UUID, String) -> VersionDTO
toVersionDTO (uuid, version) =
  VersionDTO
    { uuid = uuid
    , version = version
    }
