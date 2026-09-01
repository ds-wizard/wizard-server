module Shared.Api.Resource.DocumentTemplate.DocumentTemplateStateJM where

import Data.Aeson

import Shared.Model.DocumentTemplate.DocumentTemplateState

instance FromJSON DocumentTemplateState

instance ToJSON DocumentTemplateState
