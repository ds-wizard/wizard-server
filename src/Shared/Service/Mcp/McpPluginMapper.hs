module Shared.Service.Mcp.McpPluginMapper where

import Codec.Archive.Zip
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

import Shared.Api.Resource.Mcp.Plugin.McpPluginClient
import Shared.Model.Mcp.McpToolset
import Shared.Model.Tenant.Tenant
import Shared.Service.Mcp.McpFlowMapper (mcpIssuer)
import Shared.Util.String (f')

mcpPluginName :: String
mcpPluginName = "fair-wizard"

mcpPluginVersion :: String
mcpPluginVersion = "1.0.0"

mcpPluginDisplayName :: String
mcpPluginDisplayName = "FAIR Wizard"

mcpPluginLogoPath :: FilePath
mcpPluginLogoPath = "assets/logo.png"

mcpPluginIcon :: String
mcpPluginIcon = "./" ++ mcpPluginLogoPath

toMcpPluginFileName :: McpPluginClient -> String
toMcpPluginFileName ClaudeMcpPluginClient = "fair-wizard-claude-plugin.zip"
toMcpPluginFileName ChatGptMcpPluginClient = "fair-wizard-chatgpt-plugin.zip"

toMcpPluginArchive :: BS.ByteString -> McpPluginClient -> Tenant -> Maybe [McpToolset] -> BSL.ByteString
toMcpPluginArchive logo client tenant mToolsets =
  let jsonEntries = fmap toJsonEntry (toMcpPluginFiles client (toMcpPluginDescription tenant mToolsets) tenant (toMcpPluginServerUrl tenant mToolsets))
      logoEntry = toEntry mcpPluginLogoPath 0 (BSL.fromStrict logo)
   in fromArchive $ foldr addEntryToArchive emptyArchive (jsonEntries ++ [logoEntry])

toMcpPluginServerUrl :: Tenant -> Maybe [McpToolset] -> String
toMcpPluginServerUrl tenant Nothing = mcpIssuer tenant
toMcpPluginServerUrl tenant (Just toolsets) = f' "%s?toolsets=%s" [mcpIssuer tenant, intercalate "," (fmap mcpToolsetName toolsets)]

toJsonEntry :: (FilePath, Value) -> Entry
toJsonEntry (path, content) = toEntry path 0 (encode content)

toMcpPluginFiles :: McpPluginClient -> String -> Tenant -> String -> [(FilePath, Value)]
toMcpPluginFiles ClaudeMcpPluginClient description tenant serverUrl =
  [ (".claude-plugin/plugin.json", toClaudePluginManifest description tenant)
  , (".mcp.json", toClaudeMcpServers serverUrl)
  ]
toMcpPluginFiles ChatGptMcpPluginClient description tenant serverUrl =
  [ ("plugin.json", toChatGptPluginManifest description tenant)
  , ("mcp.json", toChatGptMcpServers serverUrl)
  ]

toClaudePluginManifest :: String -> Tenant -> Value
toClaudePluginManifest description tenant =
  object
    [ "name" .= mcpPluginName
    , "displayName" .= mcpPluginDisplayName
    , "version" .= mcpPluginVersion
    , "description" .= description
    , "author" .= object ["name" .= mcpPluginDisplayName]
    , "homepage" .= tenant.clientUrl
    , "icon" .= mcpPluginIcon
    ]

toClaudeMcpServers :: String -> Value
toClaudeMcpServers serverUrl =
  object
    [ "mcpServers"
        .= object
          [ "fair-wizard"
              .= object
                [ "type" .= ("http" :: String)
                , "url" .= serverUrl
                ]
          ]
    ]

toChatGptPluginManifest :: String -> Tenant -> Value
toChatGptPluginManifest description tenant =
  object
    [ "$schema" .= ("https://agent-plugins.org/schemas/1.0.0/plugin.schema.json" :: String)
    , "name" .= mcpPluginName
    , "version" .= mcpPluginVersion
    , "description" .= description
    , "author" .= object ["name" .= mcpPluginDisplayName]
    , "homepage" .= tenant.clientUrl
    , "extensions"
        .= object
          [ "com.openai"
              .= object
                [ "interface"
                    .= object
                      [ "displayName" .= mcpPluginDisplayName
                      , "shortDescription" .= ("FAIR Wizard for agents" :: String)
                      , "longDescription" .= description
                      , "developerName" .= mcpPluginDisplayName
                      , "composerIcon" .= mcpPluginIcon
                      , "logo" .= mcpPluginIcon
                      ]
                ]
          ]
    ]

toChatGptMcpServers :: String -> Value
toChatGptMcpServers serverUrl =
  object
    [ "$schema" .= ("https://agent-plugins.org/schemas/1.0.0/mcp.schema.json" :: String)
    , "mcpServers"
        .= object
          [ "fair-wizard"
              .= object
                [ "type" .= ("streamable-http" :: String)
                , "url" .= serverUrl
                ]
          ]
    ]

toMcpPluginDescription :: Tenant -> Maybe [McpToolset] -> String
toMcpPluginDescription tenant mToolsets =
  f' "Lets an agent work with %s in FAIR Wizard: %s." [tenant.name, intercalate ", " (fmap toolsetDescription (fromMaybe defaultToolsets mToolsets))]
  where
    defaultToolsets = [KmMcpToolset, DtMcpToolset, ProjectsMcpToolset, AdminMcpToolset]

toolsetDescription :: McpToolset -> String
toolsetDescription KmMcpToolset = "knowledge models"
toolsetDescription DtMcpToolset = "document templates"
toolsetDescription ProjectsMcpToolset = "projects and their documents"
toolsetDescription AdminMcpToolset = "administration of the organization"
toolsetDescription AnalyticsMcpToolset = "Analytics insights"
