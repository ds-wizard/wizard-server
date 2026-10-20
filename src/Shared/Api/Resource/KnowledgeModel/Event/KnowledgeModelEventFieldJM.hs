module Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM where

import Control.Monad
import Data.Aeson
import Data.Proxy

import Shared.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.Util.JsonSchema

instance FromJSON a => FromJSON (EventField a) where
  parseJSON (Object o) = do
    efChanged <- o .: "changed"
    if efChanged
      then do
        efValue <- o .: "value"
        return $ ChangedValue efValue
      else return NothingChanged
  parseJSON _ = mzero
  omittedField = Just NothingChanged

instance ToJSON a => ToJSON (EventField a) where
  toJSON (ChangedValue efValue) = object ["changed" .= True, "value" .= efValue]
  toJSON NothingChanged = object ["changed" .= False]

instance JsonSchema a => JsonSchema (EventField a) where
  jsonSchema _ =
    oneOfSchema
      [ objectSchema ["changed"] [("changed", constSchema False)]
      , objectSchema ["changed", "value"] [("changed", constSchema True), ("value", jsonSchema (Proxy :: Proxy a))]
      ]
  mayBeOmitted _ = True
