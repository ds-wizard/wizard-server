module Shared.Api.Resource.Document.DocumentSM where

import Data.Swagger

import Shared.Api.Resource.Document.DocumentDTO
import Shared.Api.Resource.Document.DocumentJM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateWithCoordinateSM ()
import Shared.Api.Resource.DocumentTemplate.WizardDocumentTemplateSimpleSM ()
import Shared.Api.Resource.Project.ProjectSimpleSM ()
import Shared.Api.Resource.Submission.SubmissionSM ()
import Shared.Database.Migration.Development.Document.Data.Documents
import Shared.Model.Document.Document
import Shared.Util.Swagger

instance ToSchema DocumentState

instance ToSchema DocumentDTO where
  declareNamedSchema = toSwagger doc1Dto
