module Postgres.Simple where

import           Utils

class Crud a where
  type DbKey a
  type DbConn a
  insert :: DbConn a -> a -> IO (Model a)
  findOne :: DbConn a -> DbKey a -> IO (Maybe (Model a))
  findAll :: DbConn a -> IO [Model a]
  update :: DbConn a -> Model a -> IO ()
  delete :: DbConn a -> DbKey a -> IO a
