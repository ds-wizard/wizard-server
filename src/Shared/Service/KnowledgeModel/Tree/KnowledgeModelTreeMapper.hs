module Shared.Service.KnowledgeModel.Tree.KnowledgeModelTreeMapper where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelEntityNodeDTO
import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelTreeDTO
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelEntity
import Shared.Model.KnowledgeModel.KnowledgeModelEntityKind
import Shared.Model.KnowledgeModel.KnowledgeModelUtil
import Shared.Model.KnowledgeModel.Tree.KnowledgeModelEntityNodeQuery
import Shared.Model.KnowledgeModel.Tree.KnowledgeModelTreeQuery

toTreeDTO :: KnowledgeModel -> U.UUID -> KnowledgeModelTreeQuery -> KnowledgeModelTreeDTO
toTreeDTO km rootUuid query =
  KnowledgeModelTreeDTO
    { knowledgeModelUuid = km.uuid
    , entities = toEntityNodes km (KnowledgeModelEntityNodeQuery query.depth query.content) [rootUuid]
    }

toEntityNodes :: KnowledgeModel -> KnowledgeModelEntityNodeQuery -> [U.UUID] -> [KnowledgeModelEntityNodeDTO]
toEntityNodes km query uuids = concatMap (subtreeNodes km parentMap query 0) (filter isRoot existingUuids)
  where
    existingUuids = L.nub . filter (entityExists km) $ uuids
    parentMap = makeParentMap km
    isRoot uuid = not . any (`elem` existingUuids) $ getAncestorUuids parentMap uuid

subtreeNodes :: KnowledgeModel -> KMParentMap -> KnowledgeModelEntityNodeQuery -> Int -> U.UUID -> [KnowledgeModelEntityNodeDTO]
subtreeNodes km parentMap query depth uuid =
  case findEntity km uuid of
    Just entity -> toEntityNodeDTO km parentMap query.content depth uuid entity : concatMap (subtreeNodes km parentMap query (depth + 1)) children
    Nothing -> []
  where
    children
      | maybe True (depth <) query.depth = getChildUuids km uuid
      | otherwise = []

toEntityNodeDTO :: KnowledgeModel -> KMParentMap -> Bool -> Int -> U.UUID -> KnowledgeModelEntity -> KnowledgeModelEntityNodeDTO
toEntityNodeDTO km parentMap withContent depth uuid entity =
  KnowledgeModelEntityNodeDTO
    { uuid = uuid
    , parentUuid = M.lookup uuid parentMap
    , entityType = entityKind entity
    , title = entityTitle km entity
    , depth = depth
    , content = if withContent then entityContent entity else Nothing
    }

entityContent :: KnowledgeModelEntity -> Maybe KnowledgeModelEntity
entityContent KnowledgeModelEntity = Nothing
entityContent entity = Just entity
