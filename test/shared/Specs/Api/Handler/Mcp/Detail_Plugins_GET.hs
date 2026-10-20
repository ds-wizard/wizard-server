module Specs.Api.Handler.Mcp.Detail_Plugins_GET (
  detail_plugins_GET,
) where

import Codec.Archive.Zip
import Data.Aeson (Value (..), decode, object, (.=))
import Data.Bifunctor (second)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import WizardServer.Constant.Mcp.PluginLogo
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Mcp.Common

-- ------------------------------------------------------------------------
-- GET /api/mcp/plugins/{client}
-- ------------------------------------------------------------------------
detail_plugins_GET :: RequestContext -> SpecWith ((), Application)
detail_plugins_GET requestContext =
  describe "GET /api/mcp/plugins/{client}" $ do
    test_200 requestContext
    test_400 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/mcp/plugins/claude"

reqHeaders = []

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK"
    reqUrl
    "fair-wizard-claude-plugin.zip"
    [ (".claude-plugin/plugin.json", claudeManifest defaultDescription)
    , (".mcp.json", claudeMcpServers mcpIssuer)
    ]
  create_test_200
    "HTTP 200 OK if the claude plugin is narrowed to toolsets"
    "/api/mcp/plugins/claude?toolsets=km,dt"
    "fair-wizard-claude-plugin.zip"
    [ (".claude-plugin/plugin.json", claudeManifest "Lets an agent work with Default Tenant in FAIR Wizard: knowledge models, document templates.")
    , (".mcp.json", claudeMcpServers (mcpIssuer ++ "?toolsets=km,dt"))
    ]
  create_test_200
    "HTTP 200 OK if the client is chatgpt"
    "/api/mcp/plugins/chatgpt"
    "fair-wizard-chatgpt-plugin.zip"
    [ ("plugin.json", chatGptManifest defaultDescription)
    , ("mcp.json", chatGptMcpServers mcpIssuer)
    ]
  create_test_200
    "HTTP 200 OK if the chatgpt plugin is narrowed to toolsets"
    "/api/mcp/plugins/chatgpt?toolsets=analytics"
    "fair-wizard-chatgpt-plugin.zip"
    [ ("plugin.json", chatGptManifest "Lets an agent work with Default Tenant in FAIR Wizard: Analytics insights.")
    , ("mcp.json", chatGptMcpServers (mcpIssuer ++ "?toolsets=analytics"))
    ]

create_test_200 title reqUrl fileName expFiles =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = ["Content-Type" <:> "application/zip", "Content-Disposition" <:> BS.pack ("attachment; filename=\"" ++ fileName ++ "\"")]
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyContainsPluginFiles expFiles}
      response `shouldRespondWith` responseMatcher

bodyContainsPluginFiles :: [(FilePath, Value)] -> MatchBody
bodyContainsPluginFiles expFiles =
  MatchBody $ \_ body ->
    let archive = toArchive body
        files = fmap (\(path, _) -> (path, decode . fromEntry =<< findEntryByPath path archive)) expFiles
        logo = fromEntry <$> findEntryByPath "assets/logo.png" archive
     in if filesInArchive archive == fmap fst expFiles ++ ["assets/logo.png"] && files == fmap (second Just) expFiles && logo == Just (BSL.fromStrict mcpPluginLogo)
          then Nothing
          else Just $ "archive mismatch:\n  expected: " ++ show expFiles ++ "\n  but got:  " ++ show (filesInArchive archive, files)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  create_test_400 "HTTP 400 BAD REQUEST if the client is unknown" "/api/mcp/plugins/copilot"
  create_test_400 "HTTP 400 BAD REQUEST if a toolset is unknown" "/api/mcp/plugins/claude?toolsets=km,unknown"

create_test_400 title reqUrl =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
defaultDescription :: String
defaultDescription = "Lets an agent work with Default Tenant in FAIR Wizard: knowledge models, document templates, projects and their documents, administration of the organization."

claudeManifest :: String -> Value
claudeManifest pluginDescription =
  object
    [ "name" .= ("fair-wizard" :: String)
    , "displayName" .= ("FAIR Wizard" :: String)
    , "version" .= ("1.0.0" :: String)
    , "description" .= pluginDescription
    , "author" .= object ["name" .= ("FAIR Wizard" :: String)]
    , "homepage" .= ("http://localhost:8080" :: String)
    , "icon" .= ("./assets/logo.png" :: String)
    ]

claudeMcpServers :: String -> Value
claudeMcpServers url =
  object
    [ "mcpServers"
        .= object
          ["fair-wizard" .= object ["type" .= ("http" :: String), "url" .= url]]
    ]

chatGptManifest :: String -> Value
chatGptManifest pluginDescription =
  object
    [ "$schema" .= ("https://agent-plugins.org/schemas/1.0.0/plugin.schema.json" :: String)
    , "name" .= ("fair-wizard" :: String)
    , "version" .= ("1.0.0" :: String)
    , "description" .= pluginDescription
    , "author" .= object ["name" .= ("FAIR Wizard" :: String)]
    , "homepage" .= ("http://localhost:8080" :: String)
    , "extensions"
        .= object
          [ "com.openai"
              .= object
                [ "interface"
                    .= object
                      [ "displayName" .= ("FAIR Wizard" :: String)
                      , "shortDescription" .= ("FAIR Wizard for agents" :: String)
                      , "longDescription" .= pluginDescription
                      , "developerName" .= ("FAIR Wizard" :: String)
                      , "composerIcon" .= ("./assets/logo.png" :: String)
                      , "logo" .= ("./assets/logo.png" :: String)
                      ]
                ]
          ]
    ]

chatGptMcpServers :: String -> Value
chatGptMcpServers url =
  object
    [ "$schema" .= ("https://agent-plugins.org/schemas/1.0.0/mcp.schema.json" :: String)
    , "mcpServers" .= object ["fair-wizard" .= object ["type" .= ("streamable-http" :: String), "url" .= url]]
    ]
