module Specs.Api.Handler.DocumentTemplate.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.DocumentTemplate.Detail_DELETE
import Specs.Api.Handler.DocumentTemplate.Detail_GET
import Specs.Api.Handler.DocumentTemplate.Detail_Locales_Content_GET
import Specs.Api.Handler.DocumentTemplate.Detail_Locales_DELETE
import Specs.Api.Handler.DocumentTemplate.Detail_Locales_GET
import Specs.Api.Handler.DocumentTemplate.Detail_Locales_POST
import Specs.Api.Handler.DocumentTemplate.Detail_Locales_Template_GET
import Specs.Api.Handler.DocumentTemplate.Detail_PUT
import Specs.Api.Handler.DocumentTemplate.Detail_Pull_POST
import Specs.Api.Handler.DocumentTemplate.List_All_GET
import Specs.Api.Handler.DocumentTemplate.List_DELETE
import Specs.Api.Handler.DocumentTemplate.List_GET
import Specs.Api.Handler.DocumentTemplate.List_Suggestions_GET

documentTemplateAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "DOCUMENT TEMPLATE API Spec" $ do
      list_GET requestContext
      list_all_GET requestContext
      list_suggestions_GET requestContext
      list_DELETE requestContext
      detail_GET requestContext
      detail_PUT requestContext
      detail_DELETE requestContext
      detail_pull_POST requestContext
      detail_locales_GET requestContext
      detail_locales_POST requestContext
      detail_locales_template_GET requestContext
      detail_locales_content_GET requestContext
      detail_locales_DELETE requestContext
