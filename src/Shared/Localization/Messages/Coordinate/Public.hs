module Shared.Localization.Messages.Coordinate.Public where

import Shared.Model.Localization.LocaleRecord

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- Format
_ERROR_VALIDATION__INVALID_COORDINATE_FORMAT =
  LocaleRecord "error.validation.invalid_coordinate_format" "Coordinate is not in the valid format" []

_ERROR_VALIDATION__INVALID_COORDINATE_VERSION_FORMAT =
  LocaleRecord "error.validation.invalid_coordinate_version_format" "Version is not in the valid format" []

_ERROR_VALIDATION__INVALID_COORDINATE_PART_FORMAT coordinatePartName coordinatePart =
  LocaleRecord "error.validation.invalid_coordinate_part_format" "%s '%s' is not in the valid format" [coordinatePartName, coordinatePart]
