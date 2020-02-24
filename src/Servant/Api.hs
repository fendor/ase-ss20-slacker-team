module Servant.Api where

import           Data.Aeson
import           GHC.Generics
import           GHC.TypeLits
import           Network.Wai.Handler.Warp
import           Servant
import           Types

type HorseApi =
       "horse" :> Get '[JSON] [Model Horse]
  :<|> "horse" :> ReqBody '[JSON] Horse :> Post '[JSON] (Model Horse)
  :<|> "horse" :> Capture "horseId" Int :> ReqBody '[JSON] Horse :> Patch '[JSON] ()
  :<|> "horse" :> Capture "horseId" Int :> Delete '[JSON] ()
  :<|> "horse" :> Capture "horseId" Int :> Get '[JSON] (Model Horse)

type JockeyApi =
       "jockey" :> Get '[JSON] [Model Jockey]
  :<|> "jockey" :> ReqBody '[JSON] Jockey :> Post '[JSON] (Model Jockey)
  :<|> "jockey" :> Capture "jockeyId" Int :> ReqBody '[JSON] Jockey :> Patch '[JSON] ()
  :<|> "jockey" :> Capture "jockeyId" Int :> Delete '[JSON] ()
  :<|> "jockey" :> Capture "jockeyId" Int :> Get '[JSON] (Model Jockey)

type RaceApi =
       "race" :> Get '[JSON] [Int]
  :<|> "race" :> Capture "raceId" Int :> Get '[JSON] [Model RaceEntry]
  :<|> "race" :> Post '[JSON] (Int, [(Int, Int)])

horseApi :: Server HorseApi
horseApi =
  findAll
  :<|> create
  :<|> update
  :<|> delete
  :<|> findOne
  where
    findAll :: Handler [Model Horse]
    findAll = undefined
    create :: Horse -> Handler (Model Horse)
    create = undefined
    update :: Int -> Horse -> Handler ()
    update = undefined
    delete :: Int -> Handler ()
    delete = undefined
    findOne :: Int -> Handler (Model Horse)
    findOne = undefined