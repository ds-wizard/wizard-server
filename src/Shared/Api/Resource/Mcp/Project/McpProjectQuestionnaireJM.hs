module Shared.Api.Resource.Mcp.Project.McpProjectQuestionnaireJM where

import Data.Aeson
import qualified Data.Map.Strict as M
import Data.Proxy
import qualified Data.UUID as U

import Shared.Api.Resource.Mcp.Project.McpProjectDTO
import Shared.Api.Resource.Mcp.Project.McpProjectJM ()
import Shared.Api.Resource.Mcp.Project.McpProjectQuestionnaireDTO
import Shared.Api.Resource.Project.ProjectReplyJM ()
import Shared.Model.Project.ProjectReply
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance ToJSON McpProjectQuestionnaireDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema McpProjectQuestionnaireDTO where
  jsonSchema _ =
    objectSchema
      ["project", "phaseUuid", "replies", "labels"]
      [ ("project", jsonSchema (Proxy :: Proxy McpProjectDTO))
      , ("phaseUuid", jsonSchema (Proxy :: Proxy (Maybe U.UUID)))
      , ("replies", jsonSchema (Proxy :: Proxy (M.Map String Reply)))
      , ("labels", jsonSchema (Proxy :: Proxy (M.Map String [U.UUID])))
      ]
