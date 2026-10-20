module WizardServer.Constant.Mcp.PluginLogo where

import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)

mcpPluginLogo :: BS.ByteString
mcpPluginLogo = $(embedFile "src/WizardServer/Constant/Mcp/plugin-logo.png")
