module Specs.Service.Mcp.Tool.KnowledgeModelEventTools.Common where

import Data.Aeson (Value (..), decode)
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.Wai.Test (SResponse (..))

import SharedTest.Specs.Api.Mcp

-- --------------------------------
-- HELPERS
-- --------------------------------
outlineOf :: Value -> Maybe [Value]
outlineOf content = arrayOf =<< valueField "thirdPartyContent" =<< valueField "outline" content

arrayOf :: Value -> Maybe [Value]
arrayOf (Array items) = Just (V.toList items)
arrayOf _ = Nothing

errorTextOf :: SResponse -> Maybe String
errorTextOf response = do
  Just [content] <- return (arrayOf =<< resultField "content" =<< decode (simpleBody response))
  String text <- valueField "text" content
  return (T.unpack text)
