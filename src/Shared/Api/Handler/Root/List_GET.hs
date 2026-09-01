module Shared.Api.Handler.Root.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.ServerContext
import Shared.Model.Error.Error
import Shared.Util.String

type List_GET = Get '[SafeJSON] (Headers '[] NoContent)

list_GET :: ServerContextC s sc m => String -> m (Headers '[] NoContent)
list_GET app = throwError =<< sendError (MovedPermanentlyError . f' "/%s-api" $ [app])
