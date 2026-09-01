module Specs.Service.Project.Migration.Migrator.SanitizerSpec where

import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.ProjectReplies
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Model.Project.Event.ProjectEventList
import Shared.Model.Project.ProjectReply
import Shared.Service.Project.Migration.Migrator.Sanitizer

import Specs.Common

sanitizerIntegrationSpec requestContext =
  describe "Sanitizer" $
    describe "sanitizeProjectEvents" $
      it "Succeed" $
        -- GIVEN:
        do
          let oldKm = km1WithQ4
          let newKm =
                putInQuestionsM question1.uuid question1WithNewType'
                  . putInQuestionsM question9.uuid question9WithNewType'
                  $ km1WithQ4
          let projectEvents = fEventsList project1Uuid
          -- WHEN:
          (Right result) <- runInContext (sanitizeProjectEvents oldKm newKm projectEvents) requestContext
          -- THEN:
          extractEventPath (head result) `shouldBe` fst rQ1
          extractEventPath (result !! 1) `shouldBe` fst rQ9
          extractSetEventValue (result !! 1) `shouldBe` (snd rQ9WithNewType).value

extractEventPath :: ProjectEventList -> String
extractEventPath (ClearReplyEventList' event) = event.path
extractEventPath (SetReplyEventList' event) = event.path
extractEventPath _ = error "Expected ClearReplyEventList' or SetReplyEventList'"

extractSetEventValue :: ProjectEventList -> ReplyValue
extractSetEventValue (SetReplyEventList' event) = event.value
extractSetEventValue _ = error "Expected SetReplyEventList'"
