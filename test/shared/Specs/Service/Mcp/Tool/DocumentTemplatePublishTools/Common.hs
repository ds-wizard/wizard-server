module Specs.Service.Mcp.Tool.DocumentTemplatePublishTools.Common where

import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Util.Uuid

-- --------------------------------
-- FIXTURES
-- --------------------------------
newerRelease :: DocumentTemplate
newerRelease = wizardDocumentTemplate {uuid = u' "8f8a9e0e-3a51-4a59-9d4b-4d1c5bd0b1a7", version = "3.0.0"}
