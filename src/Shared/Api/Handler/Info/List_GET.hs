module Shared.Api.Handler.Info.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Info.InfoDTO
import Shared.Api.Resource.Info.InfoJM ()
import Shared.Constant.DocumentTemplate
import Shared.Constant.KnowledgeModel
import Shared.Model.Context.TransactionState
import Shared.Service.Info.InfoService

type List_GET =
  Header "Host" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] InfoDTO)

list_GET :: WizardHandlerC s sm r rm => Maybe String -> sm (Headers '[Header "x-trace-uuid" String] InfoDTO)
list_GET mServerUrl =
  runInUnauthService mServerUrl NoTransaction $
    addTraceUuidHeader =<< do
      let metamodelVersions =
            [ InfoMetamodelVersionDTO {name = "Knowledge Model", version = show knowledgeModelMetamodelVersion}
            , InfoMetamodelVersionDTO {name = "Document Template", version = show documentTemplateMetamodelVersion}
            ]
      getInfo metamodelVersions
