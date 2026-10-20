module Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.WebsocketSpec where

import Test.Hspec hiding (shouldBe)

import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.DeliverySpec
import Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.EventBatchSpec
import Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.GeneralSpec
import Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.SetEventSpec
import Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.SetRepliesSpec

knowledgeModelEditorWebsocketAPI :: RequestContext -> SpecWith ()
knowledgeModelEditorWebsocketAPI requestContext =
  describe "WS /api/knowledge-model-editors/{uuid}/websocket" $ do
    generalSpec requestContext
    deliverySpec requestContext
    setEventSpec requestContext
    setRepliesSpec requestContext
    eventBatchSpec requestContext
