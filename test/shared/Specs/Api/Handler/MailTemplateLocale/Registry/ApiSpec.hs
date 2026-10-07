module Specs.Api.Handler.MailTemplateLocale.Registry.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Integration.Http.Registry.Common

import Specs.Api.Handler.MailTemplateLocale.Registry.Detail_Pull_POST
import Specs.Api.Handler.MailTemplateLocale.Registry.List_GET

mailTemplateLocaleRegistryAPI serverContext requestContext = do
  fakeServerContext <- runIO $ withFakeRegistryServer serverContext
  with (startWebApp fakeServerContext requestContext) $
    describe "MAIL TEMPLATE LOCALE REGISTRY API Spec" $ do
      list_GET requestContext
      detail_pull_POST requestContext
