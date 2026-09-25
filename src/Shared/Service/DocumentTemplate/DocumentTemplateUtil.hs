module Shared.Service.DocumentTemplate.DocumentTemplateUtil where

import qualified Data.UUID as U

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Model.Context.RequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Util.List (groupBy)

groupDocumentTemplates :: [DocumentTemplate] -> [[DocumentTemplate]]
groupDocumentTemplates =
  groupBy (\t1 t2 -> t1.organizationId == t2.organizationId && t1.templateId == t2.templateId)

resolveDocumentTemplateCoordinate :: RequestContextC s sc m => Coordinate -> Maybe U.UUID -> m DocumentTemplate
resolveDocumentTemplateCoordinate coordinate mWorkspaceUuid =
  if coordinate.version == "latest"
    then findLatestDocumentTemplateByOrganizationIdAndTemplateId coordinate.organizationId coordinate.entityId mWorkspaceUuid
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
                (steps f)
          , tenantUuid = tenantUuid
          }
          :: DocumentTemplateFormat
    )
