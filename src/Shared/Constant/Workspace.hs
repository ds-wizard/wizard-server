module Shared.Constant.Workspace where

import qualified Data.UUID as U

import Shared.Util.Uuid

defaultWorkspaceUuid :: U.UUID
defaultWorkspaceUuid = u' "7a1c8e2f-3b4d-4c5e-9f60-1a2b3c4d5e6f"

secondWorkspaceUuid :: U.UUID
secondWorkspaceUuid = u' "3f2b1c0d-9e8f-4a7b-b6c5-d4e3f2a1b0c9"

differentWorkspaceUuid :: U.UUID
differentWorkspaceUuid = u' "0b6e4d3c-2a1f-4e8d-8c7b-9a5f4e3d2c1b"
