module Specs.Model.PersistentCommand.Mail.MailCommandSpec where

import Data.Aeson
import qualified Data.Map.Strict as M
import Test.Hspec

import Shared.Model.PersistentCommand.Mail.MailCommand

mailCommandSpec =
  describe "MailCommand" $
    it "carries sendAsOne in its JSON body" $ do
      let command = MailCommand {mode = "wizard", template = "resetPassword", recipients = [MailRecipient Nothing "albert.einstein@example.com"], parameters = M.empty, sendAsOne = False}
      toJSON command
        `shouldBe` object
          [ "mode" .= String "wizard"
          , "template" .= String "resetPassword"
          , "recipients" .= [object ["uuid" .= Null, "email" .= String "albert.einstein@example.com"]]
          , "parameters" .= object []
          , "sendAsOne" .= False
          ]
