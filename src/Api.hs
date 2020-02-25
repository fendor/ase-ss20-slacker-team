module Api where

import           Servant
import           Servant.Swagger.UI
import           Servant.Swagger
import           Types
import           Polysemy
import           Database.PostgreSQL.Simple
import qualified Postgres.Simple               as Db
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Lens.Operators
import           Data.Swagger.Lens

type HorseApi = "horse"  :>
  (    Summary "Get all entities"  :> Get '[JSON] [Model Horse]
  :<|> Summary "Create new entity" :> ReqBody '[JSON] Horse :> Post '[JSON] (Model Horse)
  :<|> Summary "Update entity"     :> Capture "horseId" Int :> ReqBody '[JSON] Horse :> Patch '[JSON] ()
  :<|> Summary "Delete entity"     :> Capture "horseId" Int :> Delete '[JSON] ()
  :<|> Summary "Find entity by id" :> Capture "horseId" Int :> Get '[JSON] (Model Horse)
  )

type JockeyApi = "jockey" :>
  (    Summary "Get all entities"   :> Get '[JSON] [Model Jockey]
  :<|> Summary "Create new entity"  :> ReqBody '[JSON] Jockey :> Post '[JSON] (Model Jockey)
  :<|> Summary "Update entity"      :> Capture "jockeyId" Int :> ReqBody '[JSON] Jockey :> Patch '[JSON] ()
  :<|> Summary "Delete entity"      :> Capture "jockeyId" Int :> Delete '[JSON] ()
  :<|> Summary "Find entity by id"  :> Capture "jockeyId" Int :> Get '[JSON] (Model Jockey)
  )

type RaceApi =
       "race" :> Get '[JSON] [Int]
  :<|> "race" :> Capture "raceId" Int :> Get '[JSON] [Model RaceEntry]
  :<|> "race" :> Post '[JSON] (Int, [(Int, Int)])

horseApi :: Connection -> Server HorseApi
horseApi conn =
       findAll
  :<|> create
  :<|> update
  :<|> delete
  :<|> findOne
  where
    findAll :: Handler [Model Horse]
    findAll = liftIO $ Db.findAll conn
    create ::  Horse -> Handler (Model Horse)
    create horse = liftIO $ Db.insert conn horse
    update :: Int -> Horse -> Handler ()
    update hid horse = liftIO $ Db.update conn (Model hid horse)
    delete :: Int -> Handler ()
    delete hid = do
      (_ :: Jockey) <- liftIO $ Db.delete conn hid
      return ()
    findOne :: Int -> Handler (Model Horse)
    findOne hid =
      liftIO (Db.findOne conn hid)
        >>= \case
          Nothing -> throwError err404
          Just h -> return h

jockeyApi :: Connection -> Server JockeyApi
jockeyApi conn =
       findAll
  :<|> create
  :<|> update
  :<|> delete
  :<|> findOne
  where
    findAll :: Handler [Model Jockey]
    findAll = liftIO $ Db.findAll conn
    create :: Jockey -> Handler (Model Jockey)
    create jockey = liftIO $ Db.insert conn jockey
    update :: Int -> Jockey -> Handler ()
    update  hid jockey = liftIO $ Db.update conn (Model hid jockey)
    delete :: Int -> Handler ()
    delete hid = do
      (_ :: Horse)  <- liftIO $ Db.delete conn hid
      return ()

    findOne :: Int -> Handler (Model Jockey)
    findOne hid =
      liftIO (Db.findOne conn hid)
        >>= \case
          Nothing -> throwError err404
          Just h -> return h

type AseApi =
         HorseApi
    :<|> JockeyApi

type Api = AseApi
    :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"

api :: Connection -> Server Api
api conn = (horseApi conn :<|> jockeyApi conn)
  :<|> swaggerSchemaUIServer swaggerDoc
 where
  swaggerDoc =
    (horseDoc <> jockeyDoc)
      &  info
      .  title
      .~ "Wendys Rennstall API"
      &  info
      .  version
      .~ "0.1"
      &  info
      .  description
      ?~ "This is an API for the Wendys Rennstall service"
      &  info
      .  license
      ?~ "MIT"
  horseDoc  = toSwagger (Proxy :: Proxy HorseApi)
  jockeyDoc = toSwagger (Proxy :: Proxy JockeyApi)

aseApp :: Connection -> Application
aseApp conn = serve (Proxy :: Proxy Api) (api conn)
