module Shared.Database.Migration.Development.Project.Data.ProjectCommands where

import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.PersistentCommand.Project.CreateProjectCommand
import Shared.Model.User.User

command1 :: CreateProjectCommand
command1 =
  CreateProjectCommand
    { name = "Project 1"
    , emails =
        [ userAlbert.email
        , userNikola.email
        ]
    , knowledgeModelPackageUuid = netherlandsKmPackageV2.uuid
    , documentTemplateUuid = Just wizardDocumentTemplate.uuid
    }

command2 :: CreateProjectCommand
command2 =
  CreateProjectCommand
    { name = "Project 2"
    , emails =
        [ userAlbert.email
        , userIsaac.email
        ]
    , knowledgeModelPackageUuid = netherlandsKmPackageV2.uuid
    , documentTemplateUuid = Nothing
    }
