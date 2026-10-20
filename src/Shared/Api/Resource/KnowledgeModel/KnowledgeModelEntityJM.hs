module Shared.Api.Resource.KnowledgeModel.KnowledgeModelEntityJM where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Proxy
import qualified Data.Text as T

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelEntity
import Shared.Model.KnowledgeModel.KnowledgeModelEntityKind
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance ToJSON KnowledgeModelEntity where
  toJSON KnowledgeModelEntity = Null
  toJSON entity = toSumJSON entity

instance JsonSchema KnowledgeModelEntity where
  jsonSchema _ =
    anyOfSchema
      [ jsonSchema (Proxy :: Proxy Chapter)
      , jsonSchema (Proxy :: Proxy Question)
      , jsonSchema (Proxy :: Proxy Answer)
      , jsonSchema (Proxy :: Proxy Choice)
      , jsonSchema (Proxy :: Proxy Expert)
      , jsonSchema (Proxy :: Proxy Reference)
      , jsonSchema (Proxy :: Proxy Integration)
      , jsonSchema (Proxy :: Proxy Tag)
      , jsonSchema (Proxy :: Proxy Metric)
      , jsonSchema (Proxy :: Proxy Phase)
      , jsonSchema (Proxy :: Proxy ResourceCollection)
      , jsonSchema (Proxy :: Proxy ResourcePage)
      ]

parseEntity :: EntityKind -> Value -> Parser KnowledgeModelEntity
parseEntity kind value =
  case kind of
    ChapterEntityKind -> ChapterEntity <$> parseJSON value
    QuestionEntityKind -> QuestionEntity <$> parseJSON value
    AnswerEntityKind -> AnswerEntity <$> parseJSON value
    ChoiceEntityKind -> ChoiceEntity <$> parseJSON value
    ExpertEntityKind -> ExpertEntity <$> parseJSON value
    ReferenceEntityKind -> ReferenceEntity <$> parseJSON value
    IntegrationEntityKind -> IntegrationEntity <$> parseJSON value
    TagEntityKind -> TagEntity <$> parseJSON value
    MetricEntityKind -> MetricEntity <$> parseJSON value
    PhaseEntityKind -> PhaseEntity <$> parseJSON value
    ResourceCollectionEntityKind -> ResourceCollectionEntity <$> parseJSON value
    ResourcePageEntityKind -> ResourcePageEntity <$> parseJSON value
    _ -> return KnowledgeModelEntity

instance ToJSON EntityKind where
  toJSON = toJSON . entityKindId

instance FromJSON EntityKind where
  parseJSON = withText "EntityKind" $ \text ->
    maybe (fail "Unknown entity kind") return (lookup (T.unpack text) [(entityKindId kind, kind) | kind <- [minBound .. maxBound]])

instance JsonSchema EntityKind where
  jsonSchema _ = enumSchema (fmap entityKindId entityNodeKinds)

entityNodeKinds :: [EntityKind]
entityNodeKinds =
  [ KnowledgeModelEntityKind
  , ChapterEntityKind
  , QuestionEntityKind
  , AnswerEntityKind
  , ChoiceEntityKind
  , ExpertEntityKind
  , ReferenceEntityKind
  , TagEntityKind
  , IntegrationEntityKind
  , MetricEntityKind
  , PhaseEntityKind
  , ResourceCollectionEntityKind
  , ResourcePageEntityKind
  ]
