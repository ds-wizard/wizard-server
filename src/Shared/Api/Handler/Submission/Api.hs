module Shared.Api.Handler.Submission.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Submission.List_GET
import Shared.Api.Handler.Submission.List_POST
import Shared.Api.Handler.WizardCommon

type SubmissionAPI =
  Tags "Document Submission"
    :> ( List_GET
           :<|> List_POST
       )

submissionApi :: Proxy SubmissionAPI
submissionApi = Proxy

submissionServer :: WizardHandlerC s sm r rm => ServerT SubmissionAPI sm
submissionServer = list_GET :<|> list_POST
