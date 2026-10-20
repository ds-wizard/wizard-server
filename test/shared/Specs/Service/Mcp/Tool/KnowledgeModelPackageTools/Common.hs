module Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.Common where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.ByteArray.Encoding (Base (..), convertToBase)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Common.PageMetadataJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Api.Resource.Library.LibraryDependentsJM ()
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.Registry.Data.RegistryPackages
import qualified Shared.Database.Migration.Development.Registry.RegistryMigration as Registry
import qualified Shared.Database.Migration.Development.User.UserMigration as User
import Shared.Model.Common.PageMetadata
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
runPackageMigrations requestContext = do
  runInContextIO Registry.runMigration requestContext
  runInContextIO KnowledgeModelPackage.runMigration requestContext

runDependentMigrations requestContext = do
  runInContextIO User.runMigration requestContext
  runPackageMigrations requestContext
  runInContextIO (insertDocumentTemplate wizardDocumentTemplate) requestContext
  runInContextIO (traverse_ insertDocumentTemplateFormat wizardDocumentTemplateFormats) requestContext
  runInContextIO (insertProject project4) requestContext
  runInContextIO (insertKnowledgeModelEditor amsterdamKnowledgeModelEditor) requestContext

netherlandsPackageDetailDto :: KnowledgeModelPackageDetailDTO
netherlandsPackageDetailDto =
  toDetailDTO
    netherlandsKmPackage
    True
    [nlRegistryPackage]
    [(netherlandsKmPackage.uuid, netherlandsKmPackage.version), (netherlandsKmPackageV2.uuid, netherlandsKmPackageV2.version)]
    (Just $ "https://registry-test.ds-wizard.org/knowledge-models/" ++ show (createCoordinate netherlandsKmPackageV2))
    []

markedPackagesPage :: PageMetadata -> [KnowledgeModelPackageSimpleDTO] -> Value
markedPackagesPage metadata packages = object ["page" .= metadata, "_embedded" .= object ["knowledgeModelPackages" .= fmap markNameAndDescription packages]]

markNameAndDescription :: KnowledgeModelPackageSimpleDTO -> Value
markNameAndDescription dto =
  case toJSON dto of
    Object o -> Object (KM.insert "name" (object ["thirdPartyContent" .= dto.name]) (KM.insert "description" (object ["thirdPartyContent" .= dto.description]) o))
    json -> json

markReadme :: KnowledgeModelPackageDetailDTO -> Value
markReadme dto =
  case toJSON dto of
    Object o -> Object (KM.insert "readme" (object ["thirdPartyContent" .= dto.readme]) o)
    json -> json

catalogItem :: String -> String -> String -> String -> String -> Value
catalogItem kind catalogId name description latestVersion =
  object
    [ "kind" .= kind
    , "id" .= catalogId
    , "name" .= object ["thirdPartyContent" .= name]
    , "description" .= object ["thirdPartyContent" .= description]
    , "latestVersion" .= latestVersion
    ]

base64Content :: BSL.ByteString -> String
base64Content = BS.unpack . convertToBase Base64 . BSL.toStrict

catalogIds :: Value -> Maybe [Value]
catalogIds content =
  case valueField "packages" content of
    Just (Array items) -> traverse (valueField "id") (V.toList items)
    _ -> Nothing

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertKnowledgeModelPackageInDB :: RequestContext -> KnowledgeModelPackage -> WaiSession () ()
assertKnowledgeModelPackageInDB requestContext expPackage = do
  packageFromDb <- getOneFromDB (findPackageByUuid expPackage.uuid) requestContext
  liftIO $ packageFromDb `shouldBe` expPackage
