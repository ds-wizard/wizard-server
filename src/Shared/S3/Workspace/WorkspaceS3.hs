module Shared.S3.Workspace.WorkspaceS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.S3.Common
import Shared.Util.String (f', splitOn)

folderName = "public/workspaces"

putWorkspaceLogo :: WizardRequestContextC s m => U.UUID -> String -> Maybe String -> BS.ByteString -> m String
putWorkspaceLogo workspaceUuid fileName mContentType = createPutObjectFn (workspaceLogoObject workspaceUuid fileName) mContentType Nothing

makeWorkspaceLogoLink :: WizardRequestContextC s m => U.UUID -> String -> m String
makeWorkspaceLogoLink workspaceUuid fileName = createMakePublicLink (workspaceLogoObject workspaceUuid fileName)

removeWorkspaceLogo :: WizardRequestContextC s m => U.UUID -> String -> m ()
removeWorkspaceLogo workspaceUuid logoUrl = createRemoveObjectFn (workspaceLogoObject workspaceUuid (logoFileName logoUrl))

workspaceLogoObject :: U.UUID -> String -> String
workspaceLogoObject workspaceUuid fileName = f' "%s/%s/%s" [folderName, U.toString workspaceUuid, fileName]

logoFileName :: String -> String
logoFileName = last . splitOn "/"
