module Shared.Service.DocumentTemplate.DocumentTemplateUtil where

import qualified Data.UUID as U

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Model.Context.RequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Util.List (groupBy)

groupDocumentTemplates :: [DocumentTemplate] -> [[DocumentTemplate]]
groupDocumentTemplates =
  groupBy (\t1 t2 -> t1.id == t2.id)

resolveDocumentTemplateCoordinate :: RequestContextC s sc m => Coordinate -> Maybe U.UUID -> m DocumentTemplate
resolveDocumentTemplateCoordinate coordinate mWorkspaceUuid =
  if coordinate.version == "latest"
    then findLatestDocumentTemplateById coordinate.id mWorkspaceUuid
    else findDocumentTemplateByCoordinate coordinate mWorkspaceUuid

changeDocumentTemplateIdInFormats :: U.UUID -> U.UUID -> [DocumentTemplateFormat] -> [DocumentTemplateFormat]
changeDocumentTemplateIdInFormats documentTemplateUuid tenantUuid =
  fmap
    ( \f ->
        f
          { documentTemplateUuid = documentTemplateUuid
          , steps =
              fmap
                ( \s ->
                    s
                      { documentTemplateUuid = documentTemplateUuid
                      , tenantUuid = tenantUuid
                      }
                      :: DocumentTemplateFormatStep
                )
                f.steps
          , tenantUuid = tenantUuid
          }
          :: DocumentTemplateFormat
    )
