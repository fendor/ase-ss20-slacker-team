{-# LANGUAGE TemplateHaskell #-}
module Postgres.Polysemy where

import           Types
import           Polysemy

data DbCrud r m a where
  Insert ::r -> DbCrud r m (Model r)
  FindOne ::Int -> DbCrud r m (Maybe (Model r))
  FindAll ::DbCrud r m [Model r]
  Update ::Model r -> DbCrud r m ()
  Delete ::Int -> DbCrud r m ()

makeSem ''DbCrud
