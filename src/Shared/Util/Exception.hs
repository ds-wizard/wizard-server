module Shared.Util.Exception where

import qualified Control.Exception.Base as E

tryNonAsync :: IO a -> IO (Either E.SomeException a)
tryNonAsync action = E.try action >>= either rethrowAsync (return . Right)
  where
    rethrowAsync e = maybe (return (Left e)) (\asyncException -> E.throwIO (asyncException :: E.SomeAsyncException)) (E.fromException e)
