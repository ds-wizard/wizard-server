module Specs.Service.Mail.MailerSpec where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U
import Test.Hspec

import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.PersistentCommand.Mail.MailCommand
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Service.Mail.Mailer
import WizardServer.Model.Context.RequestContext

import Specs.Common

mailerIntegrationSpec requestContext =
  describe "Mailer Integration" $ do
    it "Sends a user mail per recipient" $ do
      -- GIVEN: Prepare expectation
      let expectation = [("registrationConfirmation", False)]
      -- WHEN:
      runInContextIO (sendRegistrationConfirmationMail userAlbert "hash" "https://example.com") requestContext
      -- THEN:
      sendAsOneFlags requestContext `shouldReturn` expectation

sendAsOneFlags :: RequestContext -> IO [(String, Bool)]
sendAsOneFlags requestContext = do
  Right commands <- runInContextIO (findPersistentCommands :: RequestContextM [PersistentCommand U.UUID]) requestContext
  return [(mail.template, mail.sendAsOne) | command <- commands, Right mail <- [eitherDecode (BSL.pack command.body) :: Either String MailCommand]]
