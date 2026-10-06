module Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePatternJM where

import Control.Monad
import Data.Aeson

import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern
import Shared.Util.Aeson
import Shared.Util.Reference

instance FromJSON KnowledgeModelPackagePattern where
  parseJSON (Object o) = do
    mId <- o .:? "id"
    mOrgId <- o .:? "orgId"
    mKmId <- o .:? "kmId"
    let id = mplus mId (joinLegacyId <$> mOrgId <*> mKmId)
    minVersion <- o .:? "minVersion"
    maxVersion <- o .:? "maxVersion"
    return KnowledgeModelPackagePattern {..}
  parseJSON _ = mzero

instance ToJSON KnowledgeModelPackagePattern where
  toJSON = genericToJSON jsonOptions
