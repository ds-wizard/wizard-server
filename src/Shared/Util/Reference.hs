module Shared.Util.Reference where

import Control.Monad.Except (throwError)
import qualified Data.List as L
import GHC.Records

import Shared.Localization.Messages.Coordinate.Public
import Shared.Model.Context.RequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Error.Error
import Shared.Util.String (splitOn)

compareVersionNeg :: String -> String -> Ordering
compareVersionNeg verA verB = compareVersion verB verA

compareVersion :: String -> String -> Ordering
compareVersion versionA versionB =
  case compare versionAMajor versionBMajor of
    LT -> LT
    GT -> GT
    EQ ->
      case compare versionAMinor versionBMinor of
        LT -> LT
        GT -> GT
        EQ ->
          case compare versionAPatch versionBPatch of
            LT -> LT
            GT -> GT
            EQ -> EQ
  where
    versionASplit = splitVersion versionA
    versionBSplit = splitVersion versionB
    versionAMajor = read (head versionASplit) :: Int
    versionAMinor = read (versionASplit !! 1) :: Int
    versionAPatch = read (versionASplit !! 2) :: Int
    versionBMajor = read (head versionBSplit) :: Int
    versionBMinor = read (versionBSplit !! 1) :: Int
    versionBPatch = read (versionBSplit !! 2) :: Int

parseCoordinate :: RequestContextC s sc m => String -> m Coordinate
parseCoordinate reference =
  case parseReference reference of
    Just coordinate -> return coordinate
    Nothing -> throwError . UserError $ _ERROR_VALIDATION__INVALID_COORDINATE_FORMAT

parseReference :: String -> Maybe Coordinate
parseReference reference =
  case splitOn ":" reference of
    [id, version] -> Just Coordinate {..}
    _ -> Nothing

parseLegacyReference :: String -> Maybe Coordinate
parseLegacyReference reference =
  case splitOn ":" reference of
    [organizationId, entityId, version] -> Just $ Coordinate {id = joinLegacyId organizationId entityId, version = version}
    _ -> parseReference reference

joinLegacyId :: String -> String -> String
joinLegacyId organizationId entityId = organizationId ++ "." ++ entityId

splitVersion :: String -> [String]
splitVersion = splitOn "."

buildReference :: String -> String -> String
buildReference id version = id ++ ":" ++ version

chooseTheNewest :: (HasField "version" a String, Ord a) => [[a]] -> [a]
chooseTheNewest = fmap (L.maximumBy (\t1 t2 -> compareVersion t1.version t2.version))
