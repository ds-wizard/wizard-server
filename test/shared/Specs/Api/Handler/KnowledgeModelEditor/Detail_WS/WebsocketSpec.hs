module Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.WebsocketSpec where

import Test.Hspec hiding (shouldBe)

import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.GeneralSpec
import Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.SetEventSpec
import Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.SetRepliesSpec

knowledgeModelEditorWebsocketAPI :: RequestContext -> SpecWith ()
knowledgeModelEditorWebsocketAPI requestContext =
  describe "WS /wizard-api/knowledge-model-editors/{uuid}/websocket" $ do
    generalSpec requestContext
    setEventSpec requestContext
    setRepliesSpec requestContext
