module Specs.Api.Handler.Project.Detail_WS.WebsocketSpec where

import Test.Hspec hiding (shouldBe)

import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Project.Detail_WS.AddCommentSpec
import Specs.Api.Handler.Project.Detail_WS.AssignCommentThreadSpec
import Specs.Api.Handler.Project.Detail_WS.ClearReplySpec
import Specs.Api.Handler.Project.Detail_WS.DeleteCommentSpec
import Specs.Api.Handler.Project.Detail_WS.DeleteCommentThreadSpec
import Specs.Api.Handler.Project.Detail_WS.EditCommentSpec
import Specs.Api.Handler.Project.Detail_WS.GeneralSpec
import Specs.Api.Handler.Project.Detail_WS.ReopenCommentThreadSpec
import Specs.Api.Handler.Project.Detail_WS.ResolveCommentThreadSpec
import Specs.Api.Handler.Project.Detail_WS.SetLabelsSpec
import Specs.Api.Handler.Project.Detail_WS.SetPhaseSpec
import Specs.Api.Handler.Project.Detail_WS.SetProjectSpec
import Specs.Api.Handler.Project.Detail_WS.SetReplySpec

projectWebsocketAPI :: RequestContext -> SpecWith ()
projectWebsocketAPI requestContext =
  describe "WS /wizard-api/projects/{projectUuid}/websocket" $ do
    generalSpec requestContext
    setReplySpec requestContext
    clearReplySpec requestContext
    setPhaseSpec requestContext
    setLabelsSpec requestContext
    resolveCommentThreadSpec requestContext
    reopenCommentThreadSpec requestContext
    assignCommentThreadSpec requestContext
    deleteCommentThreadSpec requestContext
    addCommentSpec requestContext
    editCommentSpec requestContext
    deleteCommentSpec requestContext
    setProjectSpec requestContext
