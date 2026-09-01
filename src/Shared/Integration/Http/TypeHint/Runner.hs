module Shared.Integration.Http.TypeHint.Runner (
  retrieveTypeHints,
  testRetrieveTypeHints,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Map.Strict as M

import Shared.Integration.Http.Common.HttpClient
import Shared.Integration.Http.TypeHint.RequestMapper
import Shared.Integration.Http.TypeHint.ResponseMapper
import Shared.Integration.Resource.TypeHint.TypeHintIDTO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.KnowledgeModel

retrieveTypeHints :: WizardRequestContextC s m => ApiIntegration -> M.Map String String -> M.Map String String -> String -> m (Either String [TypeHintIDTO])
retrieveTypeHints integration variables secrets q = do
  httpRequest <- liftIO $ toHttpRequest integration variables secrets q
  restrictedHttpClientManager <- asks (.restrictedHttpClientManager')
  case httpRequest of
    Left error -> return $ Left error
    Right request -> runRequestIO'With restrictedHttpClientManager request (toRetrieveTypeHintsResponse integration)

testRetrieveTypeHints :: WizardRequestContextC s m => ApiIntegration -> M.Map String String -> M.Map String String -> String -> m TypeHintExchange
testRetrieveTypeHints integration variables secrets q = do
  httpRequest <- liftIO $ toHttpRequest integration variables secrets q
  testRequest <- liftIO $ toTypeHintTestRequest integration variables secrets q
  restrictedHttpClientManager <- asks (.restrictedHttpClientManager')
  case httpRequest of
    Left error -> return $ toTypeHintTestRequestError testRequest error
    Right request -> do
      httpResponse <- runSimpleRequestWith restrictedHttpClientManager request
      case httpResponse of
        Left error -> return $ toTypeHintTestResponseError testRequest (toResponseErrorMessage error)
        Right response -> return $ toTypeHintTestResponse testRequest response
