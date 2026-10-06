module Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundlePackageJM where

import Control.Monad
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Time

import Shared.Api.Resource.Coordinate.CoordinateJM
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseJM ()
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage

instance ToJSON KnowledgeModelBundlePackage where
  toJSON pkg =
    object $
      [ "id" .= pkg.id
      , "name" .= pkg.name
      , "version" .= pkg.version
      , "phase" .= pkg.phase
      , "metamodelVersion" .= pkg.metamodelVersion
      , "description" .= pkg.description
      , "readme" .= pkg.readme
      , "license" .= pkg.license
      , "language" .= pkg.language
      , "events" .= pkg.events
      , "nonEditable" .= pkg.nonEditable
      , "createdAt" .= pkg.createdAt
      ]
        ++ coordinateToPairs "previousPackage" pkg.previousPackageId
        ++ coordinateToPairs "forkOfPackage" pkg.forkOfPackageId
        ++ coordinateToPairs "mergeCheckpointPackage" pkg.mergeCheckpointPackageId

instance FromJSON KnowledgeModelBundlePackage where
  parseJSON (Object o) = do
    id <- parseLegacyId o "kmId"
    name <- o .: "name"
    version <- o .: "version"
    let phase = ReleasedKnowledgeModelPackagePhase
    metamodelVersion <- o .: "metamodelVersion"
    description <- o .: "description"
    readme <- o .:? "readme" .!= ""
    license <- o .:? "license" .!= ""
    language <- o .:? "language" .!= "en"
    parentPackageId <- parseCoordinateFields o "parentPackage"
    previousPackageId <- mplus <$> parseCoordinateFields o "previousPackage" <*> pure parentPackageId
    forkOfPackageId <- mplus <$> parseCoordinateFields o "forkOfPackage" <*> pure parentPackageId
    mergeCheckpointPackageId <- mplus <$> parseCoordinateFields o "mergeCheckpointPackage" <*> pure parentPackageId
    eventSerialized <- o .: "events"
    events <- parseJSON eventSerialized
    let nonEditable = False
    createdAt <- o .:? "createdAt" .!= UTCTime (fromJust $ fromGregorianValid 1970 1 1) 0
    return KnowledgeModelBundlePackage {..}
  parseJSON _ = mzero
