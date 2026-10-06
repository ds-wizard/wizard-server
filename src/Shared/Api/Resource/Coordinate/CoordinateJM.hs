module Shared.Api.Resource.Coordinate.CoordinateJM where

import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.String (fromString)
import qualified Data.Text as T
import Servant.API

import Shared.Model.Coordinate.Coordinate
import Shared.Util.Reference

parseCoordinateFields :: Object -> String -> Parser (Maybe Coordinate)
parseCoordinateFields o prefix = do
  mId <- o .:? fromString (prefix ++ "Id")
  mVersion <- o .:? fromString (prefix ++ "Version")
  return $
    case (mId, mVersion) of
      (Just id, Just version) -> Just Coordinate {..}
      (Just reference, Nothing) -> parseLegacyReference reference
      _ -> Nothing

parseLegacyId :: Object -> Key -> Parser String
parseLegacyId o entityIdKey = do
  mOrganizationId <- o .:? "organizationId"
  mEntityId <- o .:? entityIdKey
  case (mOrganizationId, mEntityId) of
    (Just organizationId, Just entityId) -> return $ joinLegacyId organizationId entityId
    _ -> o .: "id"

coordinateToPairs :: String -> Maybe Coordinate -> [Pair]
coordinateToPairs prefix mCoordinate =
  [ fromString (prefix ++ "Id") .= fmap (.id) mCoordinate
  , fromString (prefix ++ "Version") .= fmap (.version) mCoordinate
  ]

instance FromHttpApiData Coordinate where
  parseQueryParam reference =
    case parseReference (T.unpack reference) of
      Just coordinate -> Right coordinate
      Nothing -> Left . T.pack $ "Unable to parse Coordinate '" ++ T.unpack reference ++ "'"

instance ToHttpApiData Coordinate where
  toUrlPiece = T.pack . show

instance FromHttpApiData [Coordinate] where
  parseQueryParam param =
    mapM (parseQueryParam . T.strip) (T.splitOn "," param)

instance ToHttpApiData [Coordinate] where
  toUrlPiece coords =
    T.intercalate "," (map toUrlPiece coords)
