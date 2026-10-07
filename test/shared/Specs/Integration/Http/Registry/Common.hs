module Specs.Integration.Http.Registry.Common where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import qualified Data.ByteString.Char8 as BS
import Network.Wai.Handler.Warp (defaultSettings, openFreePort, runSettingsSocket)
import Servant hiding (ServerContext)

import qualified RegistryPublic.Api.Handler.Locale.Detail_Po_GET as LOC_Detail_Po_GET
import qualified RegistryPublic.Api.Handler.Locale.List_GET as LOC_List_GET
import qualified RegistryPublic.Api.Handler.MailTemplateLocale.Detail_Po_GET as MTL_Detail_Po_GET
import qualified RegistryPublic.Api.Handler.MailTemplateLocale.List_GET as MTL_List_GET
import RegistryPublic.Api.Resource.Locale.LocaleDTO
import Shared.Api.Handler.Common
import Shared.Database.Migration.Development.Registry.Data.RegistryLocales
import Shared.Integration.Http.Common.ServantClient
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Config.WizardServerConfig
import WizardServer.Model.Context.RequestContext
import WizardServer.Model.Context.ServerContext

type FakeRegistryAPI =
  "api"
    :> ( LOC_List_GET.List_GET
           :<|> LOC_Detail_Po_GET.Detail_Po_GET
           :<|> MTL_List_GET.List_GET
           :<|> MTL_Detail_Po_GET.Detail_Po_GET
       )

withFakeRegistry :: RequestContext -> IO RequestContext
withFakeRegistry context = do
  registryClient <- startFakeRegistry context.serverConfig context.httpClientManager context.buildInfoConfig.releaseVersion
  return (context {registryClient = registryClient} :: RequestContext)

withFakeRegistryServer :: ServerContext -> IO ServerContext
withFakeRegistryServer context = do
  registryClient <- startFakeRegistry context.serverConfig context.httpClientManager context.buildInfoConfig.releaseVersion
  return (context {registryClient = registryClient} :: ServerContext)

startFakeRegistry serverConfig manager appVersion = do
  (port, socket) <- openFreePort
  void . forkIO . runSettingsSocket defaultSettings socket $ serve (Proxy :: Proxy FakeRegistryAPI) (fakeRegistryServer appVersion)
  let registryConfig = serverConfig.registry {url = "http://localhost:" ++ show port} :: ServerConfigRegistry
  createRegistryClient (serverConfig {registry = registryConfig} :: ServerConfig) manager

fakeRegistryServer :: String -> Server FakeRegistryAPI
fakeRegistryServer appVersion =
  listLocales appVersion registryLocaleDtos
    :<|> downloadPo [("de", deRegistryLocalePo), ("nl", nlRegistryLocalePo)]
    :<|> listLocales appVersion registryMailTemplateLocaleDtos
    :<|> downloadPo [("de", deRegistryMailTemplateLocalePo)]

listLocales :: String -> [LocaleDTO] -> Maybe String -> Maybe String -> Handler (Headers '[Header "x-trace-uuid" String] [LocaleDTO])
listLocales appVersion locales _ mAppVersion =
  return . addHeader "" $ if mAppVersion == Just appVersion then locales else []

downloadPo :: [(String, BS.ByteString)] -> Maybe String -> String -> String -> Handler (Headers '[Header "x-trace-uuid" String] FileStream)
downloadPo pos _ code "5.0.0" = maybe (throwError err404) (return . addHeader "" . FileStream) (lookup code pos)
downloadPo _ _ _ _ = throwError err404

registryLocaleDtos :: [LocaleDTO]
registryLocaleDtos =
  [ LocaleDTO {code = "de", name = "Deutsch", version = "5.0.0"}
  , LocaleDTO {code = "nl", name = "Nederlands", version = "5.0.0"}
  ]

registryMailTemplateLocaleDtos :: [LocaleDTO]
registryMailTemplateLocaleDtos = [LocaleDTO {code = "de", name = "Deutsch", version = "5.0.0"}]
