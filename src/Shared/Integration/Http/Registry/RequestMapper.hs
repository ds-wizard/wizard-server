module Shared.Integration.Http.Registry.RequestMapper where

import Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Map.Strict as M
import Servant
import Servant.Client
import Prelude hiding (lookup)

import qualified RegistryPublic.Api.Handler.DocumentTemplate.List_GET as TML_List_GET
import qualified RegistryPublic.Api.Handler.KnowledgeModelPackage.List_Bundle_POST as PKG_List_Bundle_POST
import qualified RegistryPublic.Api.Handler.KnowledgeModelPackage.List_GET as PKG_List_GET
import qualified RegistryPublic.Api.Handler.Locale.List_GET as LOC_List_GET
import RegistryPublic.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryPublic.Api.Resource.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.Common.SemVer2TupleJM ()
import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import Shared.Constant.Api
import Shared.Constant.DocumentTemplate
import Shared.Constant.KnowledgeModel
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Http.HttpRequest
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import Shared.Model.Settings.Settings
import Shared.Model.Statistics.InstanceStatistics
import Shared.Util.String (f', splitOn)

toRetrievePackagesRequest
  :: SettingsRegistry -> InstanceStatistics -> ClientM (Headers '[Header "x-trace-uuid" String] [KnowledgeModelPackageSimpleDTO])
toRetrievePackagesRequest tenantConfig iStat =
  client
    PKG_List_GET.list_GET_Api
    mTokenHeader
    xUserCountHeaderName
    xKnowledgeModelPackageCountHeaderName
    xProjectCountHeaderName
    xKnowledgeModelEditorCountHeaderName
    xDocCountHeaderName
    xTmlCountHeaderName
    pkgId
    metamodelVersion
  where
    mTokenHeader = Just $ "Bearer " ++ tenantConfig.apiKey
    xUserCountHeaderName = Just . show $ iStat.userCount
    xKnowledgeModelPackageCountHeaderName = Just . show $ iStat.pkgCount
    xProjectCountHeaderName = Just . show $ iStat.prjCount
    xKnowledgeModelEditorCountHeaderName = Just . show $ iStat.knowledgeModelEditorCount
    xDocCountHeaderName = Just . show $ iStat.docCount
    xTmlCountHeaderName = Just . show $ iStat.tmlCount
    pkgId = Nothing
    metamodelVersion = Just knowledgeModelMetamodelVersion

toRetrieveDocumentTemplatesRequest
  :: SettingsRegistry -> ClientM (Headers '[Header "x-trace-uuid" String] [DocumentTemplateSimpleDTO])
toRetrieveDocumentTemplatesRequest tenantConfig =
  client TML_List_GET.list_GET_Api mTokenHeader tmlId metamodelVersion
  where
    mTokenHeader = Just $ "Bearer " ++ tenantConfig.apiKey
    tmlId = Nothing
    metamodelVersion = Just documentTemplateMetamodelVersion

toRetrieveLocaleRequest :: String -> SettingsRegistry -> ClientM (Headers '[Header "x-trace-uuid" String] [LocaleDTO])
toRetrieveLocaleRequest version tenantConfig =
  client LOC_List_GET.list_GET_Api mTokenHeader lclId recommendedAppVersion
  where
    mTokenHeader = Just $ "Bearer " ++ tenantConfig.apiKey
    lclId = Nothing
    recommendedAppVersion =
      case splitOn "." version of
        [major, minor, _] -> Just . f' "%s.%s.%s" $ [major, minor, "0"]
        _ -> Just "1.0.0"

toRetrieveKnowledgeModelBundleByIdRequest :: ServerConfigRegistry -> SettingsRegistry -> String -> HttpRequest
toRetrieveKnowledgeModelBundleByIdRequest serverConfig tenantConfig pkgId =
  HttpRequest
    { requestMethod = "GET"
    , requestUrl = serverConfig.url ++ apiPrefix ++ "/knowledge-model-packages/" ++ pkgId ++ "/bundle"
    , requestHeaders = M.fromList [(authorizationHeaderName, "Bearer " ++ tenantConfig.apiKey)]
    , requestBody = BS.empty
    , multipart = Nothing
    }

toRetrieveDocumentTemplateBundleByCoordinateRequest :: ServerConfigRegistry -> SettingsRegistry -> Coordinate -> HttpRequest
toRetrieveDocumentTemplateBundleByCoordinateRequest serverConfig tenantConfig coordinate =
  HttpRequest
    { requestMethod = "GET"
    , requestUrl = serverConfig.url ++ apiPrefix ++ "/document-templates/" ++ show coordinate ++ "/bundle"
    , requestHeaders = M.fromList [(authorizationHeaderName, "Bearer " ++ tenantConfig.apiKey)]
    , requestBody = BS.empty
    , multipart = Nothing
    }

toRetrieveLocaleBundleByCoordinateRequest :: ServerConfigRegistry -> SettingsRegistry -> Coordinate -> HttpRequest
toRetrieveLocaleBundleByCoordinateRequest serverConfig tenantConfig coordinate =
  HttpRequest
    { requestMethod = "GET"
    , requestUrl = serverConfig.url ++ apiPrefix ++ "/locales/" ++ show coordinate ++ "/bundle"
    , requestHeaders = M.fromList [(authorizationHeaderName, "Bearer " ++ tenantConfig.apiKey)]
    , requestBody = BS.empty
    , multipart = Nothing
    }

toUploadKnowledgeModelBundleRequest :: SettingsRegistry -> KnowledgeModelBundle -> ClientM (Headers '[Header "x-trace-uuid" String] KnowledgeModelBundle)
toUploadKnowledgeModelBundleRequest tenantConfig =
  client PKG_List_Bundle_POST.list_bundle_POST_Api mTokenHeader
  where
    mTokenHeader = Just $ "Bearer " ++ tenantConfig.apiKey

toUploadDocumentTemplateBundleRequest :: ServerConfigRegistry -> SettingsRegistry -> BSL.ByteString -> HttpRequest
toUploadDocumentTemplateBundleRequest serverConfig tenantConfig bundle =
  HttpRequest
    { requestMethod = "POST"
    , requestUrl = serverConfig.url ++ apiPrefix ++ "/document-templates/bundle"
    , requestHeaders = M.fromList [(authorizationHeaderName, "Bearer " ++ tenantConfig.apiKey)]
    , requestBody = BSL.toStrict bundle
    , multipart = Just $ HttpRequestMultipart {key = "file", fileName = Just "file.zip", contentType = Just "application/zip"}
    }

toUploadLocaleBundleRequest :: ServerConfigRegistry -> SettingsRegistry -> BSL.ByteString -> HttpRequest
toUploadLocaleBundleRequest serverConfig tenantConfig bundle =
  HttpRequest
    { requestMethod = "POST"
    , requestUrl = serverConfig.url ++ apiPrefix ++ "/locales/bundle"
    , requestHeaders = M.fromList [(authorizationHeaderName, "Bearer " ++ tenantConfig.apiKey)]
    , requestBody = BSL.toStrict bundle
    , multipart = Just $ HttpRequestMultipart {key = "file", fileName = Just "file.zip", contentType = Just "application/zip"}
    }
