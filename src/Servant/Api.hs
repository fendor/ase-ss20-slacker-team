module Servant.Api where

import           Data.Aeson
import           Data.Functor                   ( void )
import           GHC.Generics
import           GHC.TypeLits
import           Network.Wai.Handler.Warp
import           Servant
import           Types
import           Database.PostgreSQL.Simple
import qualified Postgres.Database as Db
import           Control.Monad.IO.Class         ( liftIO )

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

horseApi :: Connection -> Server HorseApi
horseApi conn =
  findAll conn
  :<|> create conn
  :<|> update conn
  :<|> delete conn
  :<|> findOne conn
  where
    findAll :: Connection -> Handler [Model Horse]
    findAll = liftIO . Db.findAll
    create :: Connection -> Horse -> Handler (Model Horse)
    create conn horse = liftIO $ Db.insert conn horse
    update :: Connection -> Int -> Horse -> Handler ()
    update conn hid horse = liftIO $ Db.update conn (Model hid horse)
    delete :: Connection -> Int -> Handler ()
    delete conn hid = do
      (_ :: Jockey) <- liftIO $ Db.delete conn hid
      return ()
    findOne :: Connection -> Int -> Handler (Model Horse)
    findOne conn hid =
      liftIO (Db.findOne conn hid)
        >>= \case
          Nothing -> throwError err404
          Just h -> return h

jockeyApi :: Connection -> Server JockeyApi
jockeyApi conn =
  findAll conn
  :<|> create conn
  :<|> update conn
  :<|> delete conn
  :<|> findOne conn
  where
    findAll :: Connection -> Handler [Model Jockey]
    findAll = liftIO . Db.findAll
    create :: Connection -> Jockey -> Handler (Model Jockey)
    create conn jockey = liftIO $ Db.insert conn jockey
    update :: Connection -> Int -> Jockey -> Handler ()
    update conn hid jockey = liftIO $ Db.update conn (Model hid jockey)
    delete :: Connection -> Int -> Handler ()
    delete conn hid = do
      (_ :: Horse)  <- liftIO $ Db.delete conn hid
      return ()

    findOne :: Connection -> Int -> Handler (Model Jockey)
    findOne conn hid =
      liftIO (Db.findOne conn hid)
        >>= \case
          Nothing -> throwError err404
          Just h -> return h

type AseApi =
  HorseApi :<|> JockeyApi

aseServer :: Proxy AseApi
aseServer = Proxy

aseApi :: Connection -> Server AseApi
aseApi conn = horseApi conn :<|> jockeyApi conn

aseApp :: Connection -> Application
aseApp conn = serve aseServer (aseApi conn)