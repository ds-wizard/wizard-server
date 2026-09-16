module Shared.Api.Handler.Root.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Constant.Api
import Shared.Model.Context.ServerContext
import Shared.Model.Error.Error

type List_GET = Get '[SafeJSON] (Headers '[] NoContent)

list_GET :: ServerContextC s sc m => m (Headers '[] NoContent)
list_GET = throwError =<< sendError (MovedPermanentlyError apiPrefix)
