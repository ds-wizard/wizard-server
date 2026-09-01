module Shared.Service.KnowledgeModel.Compiler.EventApplicator.KnowledgeModel where

import Shared.Model.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator
import Shared.Service.KnowledgeModel.Compiler.Modifier.Answer ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Chapter ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Expert ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Integration ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.KnowledgeModel ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Metric ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Modifier
import Shared.Service.KnowledgeModel.Compiler.Modifier.Phase ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Question ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Reference ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Tag ()

instance ApplyEvent AddKnowledgeModelEvent where
  apply event content _ = Right $ createEntity event content

instance ApplyEvent EditKnowledgeModelEvent where
  apply event content = Right . editEntity event content
