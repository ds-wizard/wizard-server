module Shared.Api.Resource.Project.ProjectReplySM where

import Data.Swagger

import Shared.Api.Resource.Common.AesonSM ()
import Shared.Api.Resource.Project.ProjectReplyJM ()
import Shared.Api.Resource.User.UserSuggestionSM ()
import Shared.Database.Migration.Development.Project.Data.ProjectReplies
import Shared.Model.Project.ProjectReply
import Shared.Util.Swagger

instance ToSchema Reply where
  declareNamedSchema = toSwagger (fst rQ1Updated)

instance ToSchema ReplyValue where
  declareNamedSchema = toSwagger (snd rQ1).value

instance ToSchema IntegrationReplyType where
  declareNamedSchema = toSwagger r9IntType
