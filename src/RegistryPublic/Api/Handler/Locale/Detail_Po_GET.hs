module RegistryPublic.Api.Handler.Locale.Detail_Po_GET where

import Servant

import Shared.Api.Handler.Common

type Detail_Po_GET =
  Header "Authorization" String
    :> "locales"
    :> Capture "code" String
    :> Capture "version" String
    :> "po"
    :> Get '[GettextPo] (Headers '[Header "x-trace-uuid" String] FileStream)

detail_po_GET_Api :: Proxy Detail_Po_GET
detail_po_GET_Api = Proxy
