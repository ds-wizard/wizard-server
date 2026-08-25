module Shared.Common.Model.Sentry.SentryEvent where

import Data.Aeson (Value)
import Data.Time (UTCTime)
import System.Log.Raven.Types (SentryLevel (..))

data SentryTarget = SentryTarget
  { enabled :: Bool
  , dsn :: String
  , environment :: String
  , release :: String
  }

data SentryBreadcrumb = SentryBreadcrumb
  { timestamp :: UTCTime
  , level :: String
  , category :: String
  , message :: String
  }

data SentryEvent = SentryEvent
  { logger :: String
  , level :: SentryLevel
  , message :: String
  , culprit :: Maybe String
  , serverName :: Maybe String
  , fingerprint :: [String]
  , tags :: [(String, String)]
  , extra :: [(String, Value)]
  , interfaces :: [(String, Value)]
  , breadcrumbs :: [SentryBreadcrumb]
  }

toSentryEvent :: String -> String -> SentryEvent
toSentryEvent eventLogger eventMessage =
  SentryEvent
    { logger = eventLogger
    , level = Error
    , message = eventMessage
    , culprit = Nothing
    , serverName = Nothing
    , fingerprint = []
    , tags = []
    , extra = []
    , interfaces = []
    , breadcrumbs = []
    }
