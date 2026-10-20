module Shared.Model.KnowledgeModel.KnowledgeModelEntity where

import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses

data KnowledgeModelEntity
  = KnowledgeModelEntity
  | ChapterEntity Chapter
  | QuestionEntity Question
  | AnswerEntity Answer
  | ChoiceEntity Choice
  | ExpertEntity Expert
  | ReferenceEntity Reference
  | IntegrationEntity Integration
  | TagEntity Tag
  | MetricEntity Metric
  | PhaseEntity Phase
  | ResourceCollectionEntity ResourceCollection
  | ResourcePageEntity ResourcePage
  deriving (Show, Eq, Generic)

findEntity :: KnowledgeModel -> U.UUID -> Maybe KnowledgeModelEntity
findEntity km uuid
  | uuid /= U.nil && uuid == km.uuid = Just KnowledgeModelEntity
  | otherwise =
      listToMaybe . catMaybes $
        [ ChapterEntity <$> M.lookup uuid km.entities.chapters
        , QuestionEntity <$> M.lookup uuid km.entities.questions
        , AnswerEntity <$> M.lookup uuid km.entities.answers
        , ChoiceEntity <$> M.lookup uuid km.entities.choices
        , ExpertEntity <$> M.lookup uuid km.entities.experts
        , ReferenceEntity <$> M.lookup uuid km.entities.references
        , IntegrationEntity <$> M.lookup uuid km.entities.integrations
        , TagEntity <$> M.lookup uuid km.entities.tags
        , MetricEntity <$> M.lookup uuid km.entities.metrics
        , PhaseEntity <$> M.lookup uuid km.entities.phases
        , ResourceCollectionEntity <$> M.lookup uuid km.entities.resourceCollections
        , ResourcePageEntity <$> M.lookup uuid km.entities.resourcePages
        ]

entityTitle :: KnowledgeModel -> KnowledgeModelEntity -> String
entityTitle km entity =
  case entity of
    KnowledgeModelEntity -> ""
    ChapterEntity chapter -> chapter.title
    QuestionEntity question -> getTitle question
    AnswerEntity answer -> answer.aLabel
    ChoiceEntity choice -> choice.aLabel
    ExpertEntity expert -> expert.name
    ReferenceEntity reference -> referenceTitle km reference
    IntegrationEntity integration -> integrationName integration
    TagEntity tag -> tag.name
    MetricEntity metric -> metric.title
    PhaseEntity phase -> phase.title
    ResourceCollectionEntity collection -> collection.title
    ResourcePageEntity page -> page.title

referenceTitle :: KnowledgeModel -> Reference -> String
referenceTitle km (ResourcePageReference' reference) = maybe "" (.title) (reference.resourcePageUuid >>= (`M.lookup` km.entities.resourcePages))
referenceTitle _ (URLReference' reference) = reference.aLabel
referenceTitle _ (CrossReference' reference) = reference.description

integrationName :: Integration -> String
integrationName (ApiIntegration' integration) = integration.name
integrationName (PluginIntegration' integration) = integration.name
