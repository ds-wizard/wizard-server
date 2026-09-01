module Specs.Service.Project.Compiler.ProjectCompilerServiceSpec where

import Test.Hspec

import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.ProjectLabels
import Shared.Database.Migration.Development.Project.Data.ProjectReplies
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.Project.Event.ProjectEvent
import Shared.Model.Project.ProjectContent
import Shared.Service.Project.Compiler.ProjectCompilerService
import Shared.Service.Project.Event.ProjectEventMapper

projectCompilerServiceSpec =
  describe "Project Compiler Service" $
    describe "applyEvent" $ do
      it "SetReplyEvent" $
        -- GIVEN:
        do
          let event = toEventList (sre_rQ1Updated' project1Uuid) (Just userAlbert)
          -- WHEN:
          let updatedProjectCtn = applyEvent project1Ctn event
          -- THEN:
          updatedProjectCtn.replies `shouldBe` fRepliesWithUpdated
      it "ClearReplyEvent" $
        -- GIVEN:
        do
          let event = toEventList (cre_rQ1' project1Uuid) (Just userAlbert)
          -- WHEN:
          let updatedProjectCtn = applyEvent project1Ctn event
          -- THEN:
          updatedProjectCtn.replies `shouldBe` fRepliesWithDeleted
      it "SetPhaseEvent" $
        -- GIVEN:
        do
          let event = toEventList (sphse_2' project1Uuid) (Just userAlbert)
          -- WHEN:
          let updatedProjectCtn = applyEvent project1Ctn event
          -- THEN:
          updatedProjectCtn.phaseUuid `shouldBe` (sphse_2 project1Uuid).phaseUuid
      it "SetLabelsEvent" $
        -- GIVEN:
        do
          let event = toEventList (slble_rQ2' project1Uuid) (Just userAlbert)
          -- WHEN:
          let updatedProjectCtn = applyEvent project1Ctn event
          -- THEN:
          updatedProjectCtn.labels `shouldBe` fLabelsEdited
