module Shared.Model.Context.ProgressReporter where

newtype ProgressReporter = ProgressReporter (Double -> Maybe Double -> String -> IO ())

noProgressReporter :: ProgressReporter
noProgressReporter = ProgressReporter (\_ _ _ -> return ())
