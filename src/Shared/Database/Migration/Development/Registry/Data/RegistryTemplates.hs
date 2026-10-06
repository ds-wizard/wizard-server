module Shared.Database.Migration.Development.Registry.Data.RegistryTemplates where

import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Registry.RegistryTemplate

commonWizardRegistryTemplate :: RegistryTemplate
commonWizardRegistryTemplate =
  RegistryTemplate
    { id = wizardDocumentTemplate.id
    , remoteVersion = wizardDocumentTemplate.version
    , createdAt = wizardDocumentTemplate.createdAt
    }
