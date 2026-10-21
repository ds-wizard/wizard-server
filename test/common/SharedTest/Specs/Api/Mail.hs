module SharedTest.Specs.Api.Mail where

import Data.Aeson (Value (..), eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.UUID as U
import Test.Hspec

import Shared.Model.PersistentCommand.Mail.MailCommand
import Shared.Model.PersistentCommand.PersistentCommand

data MailExpectation = MailExpectation
  { mode :: String
  , template :: String
  , recipientUuids :: [U.UUID]
  , sendAsOne :: Bool
  , parameters :: [(String, Value)]
  }
  deriving (Show, Eq)

wizardMail :: String -> U.UUID -> [(String, Value)] -> MailExpectation
wizardMail template recipientUuid parameters =
  MailExpectation {mode = "wizard", template = template, recipientUuids = [recipientUuid], sendAsOne = False, parameters = parameters}

shouldMatchMails :: [PersistentCommand U.UUID] -> [MailExpectation] -> IO ()
shouldMatchMails commands expMails =
  zipWith toMailExpectation (fmap (fmap fst . (.parameters)) expMails ++ repeat []) (decodeMails commands) `shouldBe` expMails

decodeMails :: [PersistentCommand U.UUID] -> [MailCommand]
decodeMails = fmap decodeMail . sortOn (.createdAt) . filter isMail
  where
    isMail command = command.component == "mailer" && command.function == "sendMail"
    decodeMail command = either (error . ("Undecodable mail body: " ++)) id (eitherDecode (BSL.pack command.body))

toMailExpectation :: [String] -> MailCommand -> MailExpectation
toMailExpectation keys mail =
  MailExpectation
    { mode = mail.mode
    , template = mail.template
    , recipientUuids = mapMaybe (.uuid) mail.recipients
    , sendAsOne = mail.sendAsOne
    , parameters = [(key, fromMaybe Null (M.lookup key mail.parameters)) | key <- keys]
    }
