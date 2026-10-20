module Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.Common where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM ()
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplate
import qualified Shared.Database.Migration.Development.Registry.RegistryMigration as Registry

import Specs.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
runTemplateMigrations requestContext = do
  runInContextIO DocumentTemplate.runMigration requestContext
  runInContextIO Registry.runMigration requestContext

markReadme :: DocumentTemplateDetailDTO -> Value
markReadme dto =
  case toJSON dto of
    Object o -> Object (KM.insert "readme" (object ["thirdPartyContent" .= dto.readme]) o)
    json -> json

isObject :: Value -> Bool
isObject (Object _) = True
isObject _ = False

isArray :: Value -> Bool
isArray (Array _) = True
isArray _ = False
