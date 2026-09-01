module Shared.Model.KnowledgeModel.Event.Common.CommonUtil where

class IsEmptyEvent event where
  isEmptyEvent :: event -> Bool
