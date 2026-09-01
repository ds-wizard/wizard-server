module Specs.Service.Project.Event.ProjectEventServiceSpec where

import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U
import Test.Hspec

import Shared.Constant.Tenant
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.ProjectVersions
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.Common.Lens
import Shared.Model.Project.Event.ProjectEvent
import Shared.Model.Project.Event.ProjectEventLenses ()
import Shared.Model.Project.ProjectReply
import Shared.Model.Project.Version.ProjectVersion
import Shared.Service.Project.Event.ProjectEventService
import Shared.Util.Date
import Shared.Util.Uuid

-- ---------------------------
-- TESTS
-- ---------------------------
projectEventServiceSpec =
  describe "ProjectEventService" $ do
    it "squash" $
      -- GIVEN: prepare data
      do
        let versions = [version1]
        let events =
              [ setCreatedAt q1_event1 (dt'' 2018 1 21 1)
              , setCreatedAt (cre_rQ1' project1Uuid) (dt'' 2018 1 21 2)
              , setCreatedAt q1_event2 (dt'' 2018 1 21 3)
              , setCreatedAt (sphse_1' project1Uuid) (dt'' 2018 1 21 4)
              , setCreatedAt q1_event3 (dt'' 2018 1 21 5)
              , setCreatedAt (slble_rQ1' project1Uuid) (dt'' 2018 1 21 6)
              , setCreatedAt q2_event1 (dt'' 2018 1 21 7)
              , setCreatedAt q1_event4 (dt'' 2018 1 21 8)
              , setCreatedAt q1_event5_nikola (dt'' 2018 1 21 9)
              , setCreatedAt q1_event6_anonymous1 (dt'' 2018 1 21 10)
              , setCreatedAt q1_event7_nikola (dt'' 2018 1 21 11)
              , setCreatedAt q1_event8_nikola (dt'' 2018 1 21 12)
              , setCreatedAt q2_event2 (dt'' 2018 1 22 0)
              ]
        -- AND: prepare expectation
        let expEvents =
              [ setCreatedAt (cre_rQ1' project1Uuid) (dt'' 2018 1 21 2)
              , setCreatedAt (sphse_1' project1Uuid) (dt'' 2018 1 21 4)
              , setCreatedAt (slble_rQ1' project1Uuid) (dt'' 2018 1 21 6)
              , setCreatedAt q2_event1 (dt'' 2018 1 21 7)
              , setCreatedAt q1_event4 (dt'' 2018 1 21 8)
              , setCreatedAt q1_event5_nikola (dt'' 2018 1 21 9)
              , setCreatedAt q1_event6_anonymous1 (dt'' 2018 1 21 10)
              , setCreatedAt q1_event7_nikola (dt'' 2018 1 21 11)
              , setCreatedAt q1_event8_nikola (dt'' 2018 1 21 12)
              , setCreatedAt q2_event2 (dt'' 2018 1 22 0)
              ]
        -- WHEN:
        let resultEvents = squash versions events
        -- THEN:
        resultEvents `shouldBe` expEvents
    it "squash keeps the last reply of each path before a version" $
      -- GIVEN: prepare data
      do
        let versions = [version2]
        let events =
              [ setCreatedAt contributors_event1 (dt'' 2018 1 21 1)
              , setCreatedAt contributor1_name_event (dt'' 2018 1 21 2)
              , setCreatedAt contributors_event2 (dt'' 2018 1 21 3)
              , setCreatedAt contributor2_name_event (dt'' 2018 1 21 4)
              ]
        -- AND: prepare expectation
        let expEvents = events
        -- WHEN:
        let resultEvents = squash versions events
        -- THEN:
        resultEvents `shouldBe` expEvents

-- ---------------------------
-- EVENTS
-- ---------------------------
q1_event1 :: ProjectEvent
q1_event1 =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "4b2a1d62f725"
      , path = "question1"
      , value = StringReply "question1_value_1"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = albert
      , createdAt = dt'' 2018 1 21 0
      }

q1_event2 :: ProjectEvent
q1_event2 =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "0d2b486b3231"
      , path = "question1"
      , value = StringReply "question1_value_2"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = albert
      , createdAt = dt'' 2018 1 21 1
      }

q1_event3 :: ProjectEvent
q1_event3 =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "04702766ab48"
      , path = "question1"
      , value = StringReply "question1_value_3"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = albert
      , createdAt = dt'' 2018 1 21 2
      }

q2_event1 :: ProjectEvent
q2_event1 =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "b2eb9e2aacc7"
      , path = "question2"
      , value = StringReply "question2_value_1"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = albert
      , createdAt = dt'' 2018 1 21 3
      }

q1_event4 :: ProjectEvent
q1_event4 =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "4db8b2bd8345"
      , path = "question1"
      , value = StringReply "question1_value_4"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = albert
      , createdAt = dt'' 2018 1 21 4
      }

q1_event5_nikola :: ProjectEvent
q1_event5_nikola =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "3fa10bf8bc47"
      , path = "question1"
      , value = StringReply "question1_value_5"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = nikola
      , createdAt = dt'' 2018 1 21 5
      }

q1_event6_anonymous1 :: ProjectEvent
q1_event6_anonymous1 =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "fbbb6cc6d91c"
      , path = "question1"
      , value = StringReply "question1_value_6"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = Nothing
      , createdAt = dt'' 2018 1 21 6
      }

q1_event7_nikola :: ProjectEvent
q1_event7_nikola =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "fe7cfccc9c50"
      , path = "question1"
      , value = StringReply "question1_value_7"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = nikola
      , createdAt = dt'' 2018 1 21 7
      }

q1_event8_nikola :: ProjectEvent
q1_event8_nikola =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "b9c6b1dd31f8"
      , path = "question1"
      , value = StringReply "question1_value_8"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = nikola
      , createdAt = dt'' 2018 1 21 8
      }

contributors_event1 :: ProjectEvent
contributors_event1 =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "5c9d2e0a6f11"
      , path = "contributors"
      , value = ItemListReply {ilValue = [contributor1Uuid]}
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = albert
      , createdAt = dt'' 2018 1 21 0
      }

contributor1_name_event :: ProjectEvent
contributor1_name_event =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "6a1f37c4b902"
      , path = "contributors." ++ U.toString contributor1Uuid ++ ".name"
      , value = StringReply "contributor_1"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = albert
      , createdAt = dt'' 2018 1 21 1
      }

contributors_event2 :: ProjectEvent
contributors_event2 =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "7be0481d5cc3"
      , path = "contributors"
      , value = ItemListReply {ilValue = [contributor1Uuid, contributor2Uuid]}
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = albert
      , createdAt = dt'' 2018 1 21 2
      }

contributor2_name_event :: ProjectEvent
contributor2_name_event =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "8cf159ae63d4"
      , path = "contributors." ++ U.toString contributor2Uuid ++ ".name"
      , value = StringReply "contributor_2"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = albert
      , createdAt = dt'' 2018 1 21 3
      }

contributor1Uuid :: U.UUID
contributor1Uuid = u' "b6b4b0b8-0f2f-4a08-a0d7-3b1f57e8a6f0"

contributor2Uuid :: U.UUID
contributor2Uuid = u' "f0f45c2e-3f8b-4a4c-90b6-9a9e4f5a6d21"

q2_event2 :: ProjectEvent
q2_event2 =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "a023f5ef76f7"
      , path = "question2"
      , value = StringReply "question2_value_2"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = albert
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 22) 0
      }

-- ---------------------------
-- VERSIONS
-- ---------------------------
version1 = (projectVersion1 project1Uuid) {eventUuid = getUuid q1_event7_nikola}

version2 = (projectVersion2 project1Uuid) {eventUuid = getUuid contributor1_name_event}

-- ---------------------------
-- USERS
-- ---------------------------
albert :: Maybe U.UUID
albert = Just $ u' "3e9da440-0a4f-43dc-86b0-0fe9009ae6f3"

nikola :: Maybe U.UUID
nikola = Just $ u' "dbcc9ac4-7e63-4d12-9a14-f2f918fd0a78"
