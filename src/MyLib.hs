{-# LANGUAGE OverloadedStrings #-}
module MyLib  where

import Database.PostgreSQL.Simple

hello :: IO [Int]
hello = do
  conn <- connect defaultConnectInfo { connectUser = "baldr" }
  [i] <- query_ conn "select 2 + 2"
  return i