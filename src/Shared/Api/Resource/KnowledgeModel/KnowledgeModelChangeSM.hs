module Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventSM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Util.Swagger

instance ToSchema KnowledgeModelChangeDTO where
  declareNamedSchema = toSwagger kmChange

kmChange :: KnowledgeModelChangeDTO
kmChange =
  KnowledgeModelChangeDTO
    { knowledgeModelPackageUuid = Just germanyKmPackage.uuid
    , events = []
    , tagUuids = []
    }
