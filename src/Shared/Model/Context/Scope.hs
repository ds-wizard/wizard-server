module Shared.Model.Context.Scope where

import qualified Data.UUID as U
import GHC.Generics

data Scope
  = NoScope
  | TenantScope
  | WorkspaceScope U.UUID
  deriving (Show, Eq, Generic)
