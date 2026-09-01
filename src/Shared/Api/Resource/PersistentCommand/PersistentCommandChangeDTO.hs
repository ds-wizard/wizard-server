module Shared.Api.Resource.PersistentCommand.PersistentCommandChangeDTO where

import GHC.Generics

import Shared.Model.PersistentCommand.PersistentCommand

data PersistentCommandChangeDTO = PersistentCommandChangeDTO
  { state :: PersistentCommandState
  }
  deriving (Show, Eq, Generic)
