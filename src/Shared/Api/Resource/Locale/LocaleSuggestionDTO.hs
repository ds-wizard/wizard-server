module Shared.Api.Resource.Locale.LocaleSuggestionDTO where

import qualified Data.UUID as U
import GHC.Generics

data LocaleSuggestionDTO = LocaleSuggestionDTO
  { uuid :: U.UUID
  , code :: String
  , name :: String
  , defaultLocale :: Bool
  }
  deriving (Show, Eq, Generic)
