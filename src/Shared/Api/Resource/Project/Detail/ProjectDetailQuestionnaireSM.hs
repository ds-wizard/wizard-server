module Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireSM where

import Data.Map.Strict as M
import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionSM ()
import Shared.Api.Resource.Project.Acl.ProjectPermSM ()
import Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireJM ()
import Shared.Api.Resource.Project.File.ProjectFileSimpleSM ()
import Shared.Api.Resource.Project.ProjectReplySM ()
import Shared.Api.Resource.Project.ProjectSharingSM ()
import Shared.Api.Resource.Project.ProjectVisibilitySM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.ProjectLabels
import Shared.Database.Migration.Development.Project.Data.ProjectReplies
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.Project.Project
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper
import Shared.Util.Swagger
import Shared.Util.Uuid

instance ToSchema ProjectDetailQuestionnaireDTO where
  declareNamedSchema =
    toSwagger $
      ProjectDetailQuestionnaireDTO
        { uuid = project1.uuid
        , name = project1.name
        , visibility = project1.visibility
        , sharing = project1.sharing
        , knowledgeModelPackage = toSuggestion netherlandsKmPackage
        , selectedQuestionTagUuids = project1.selectedQuestionTagUuids
        , language = Nothing
        , locale = Nothing
        , isTemplate = project1.isTemplate
        , knowledgeModel = km1
        , replies = fReplies
        , labels = fLabels
        , phaseUuid = Just . u' $ "4b376e49-1589-429b-9590-c654378f0bd5"
        , permissions = [project1AlbertEditProjectPermDto]
        , files = []
        , unresolvedCommentCounts =
            M.fromList
              [
                ( "4f61fdfa-ce82-41b5-a1e6-218beaf41660.0dc58313-eb80-4f74-a8c1-347b644665d5"
                , M.fromList [(u' "f1de85a9-7f22-4d0c-bc23-3315cc4c85d7", 4)]
                )
              ]
        , resolvedCommentCounts =
            M.fromList
              [
                ( "4f61fdfa-ce82-41b5-a1e6-218beaf41660.0dc58313-eb80-4f74-a8c1-347b644665d5"
                , M.fromList [(u' "f1de85a9-7f22-4d0c-bc23-3315cc4c85d7", 2)]
                )
              ]
        , fileCount = 0
        }
