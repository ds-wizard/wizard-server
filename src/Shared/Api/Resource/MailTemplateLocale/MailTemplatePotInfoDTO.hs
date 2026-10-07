module Shared.Api.Resource.MailTemplateLocale.MailTemplatePotInfoDTO where

import GHC.Generics

data MailTemplatePotInfoDTO = MailTemplatePotInfoDTO
  { source :: MailTemplatePotSource
  , potVersion :: Maybe String
  }
  deriving (Show, Eq, Generic)

data MailTemplatePotSource
  = DefaultMailTemplatePotSource
  | CustomMailTemplatePotSource
  | NoneMailTemplatePotSource
  deriving (Show, Eq, Generic)
