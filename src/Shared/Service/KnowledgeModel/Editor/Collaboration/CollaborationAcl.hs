module Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationAcl where

import Shared.Model.Context.WizardRequestContext
import Shared.Model.Websocket.WebsocketRecord
import Shared.Service.KnowledgeModel.Editor.EditorAcl
import Shared.Util.Uuid

checkViewPermission :: WizardRequestContextC s m => WebsocketRecord -> m ()
checkViewPermission record = checkPermissionToEditor (u' record.entityId)

checkEditPermission :: WizardRequestContextC s m => WebsocketRecord -> m ()
checkEditPermission record = checkPermissionToEditor (u' record.entityId)
