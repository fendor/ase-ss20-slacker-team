module Jockey where

import qualified Data.Text                     as T
import           Data.Aeson
import qualified Data.Aeson.Types              as Aeson
import           GHC.Generics
import           Data.Swagger.Schema
import           Data.Swagger
import           Data.Proxy
import           Polysemy
import           Polysemy.Reader
import           Polysemy.Fail
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           Control.Lens.Operators  hiding ( (.=) )
import           Data.HashMap.Strict.InsOrd     ( fromList )
import           Data.Maybe
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Instances.Text ( )
import           Generic.Random
import           Type.Reflection
import           Utils
import           Postgres.Simple as Db
import           Postgres.Polysemy (DbCrud(..))
import           Servant
import           Control.Monad.IO.Class         ( liftIO )

data Jockey = Jockey
    { jockeyName :: T.Text
    , jockeySkill :: Double
    , jockeyAge :: Int
    , jockeyDeleted :: Bool
    }
    deriving (Show, Eq, Read, Generic, Typeable)

type JockeyApi = "jockey" :>
  (    Summary "Get all entities"   :> Get '[JSON] [Model Jockey]
  :<|> Summary "Create new entity"  :> ReqBody '[JSON] Jockey :> Post '[JSON] (Model Jockey)
  :<|> Summary "Update entity"      :> Capture "jockeyId" Int :> ReqBody '[JSON] Jockey :> Patch '[JSON] ()
  :<|> Summary "Delete entity"      :> Capture "jockeyId" Int :> Delete '[JSON] ()
  :<|> Summary "Find entity by id"  :> Capture "jockeyId" Int :> Get '[JSON] (Model Jockey)
  )

jockeyApi :: Connection -> Server JockeyApi
jockeyApi conn =
       findAll'
  :<|> create'
  :<|> update'
  :<|> delete'
  :<|> findOne'
  where
    findAll' :: Handler [Model Jockey]
    findAll' = liftIO $ Db.findAll conn
    create' :: Jockey -> Handler (Model Jockey)
    create' jockey = liftIO $ Db.insert conn jockey
    update' :: Int -> Jockey -> Handler ()
    update'  hid jockey = liftIO $ Db.update conn (Model hid jockey)
    delete' :: Int -> Handler ()
    delete' hid = do
      (_ :: Jockey)  <- liftIO $ Db.delete conn hid
      return ()

    findOne' :: Int -> Handler (Model Jockey)
    findOne' hid =
      liftIO (Db.findOne conn hid)
        >>= \case
          Nothing -> throwError err404
          Just h -> return h

instance FromRow Jockey
instance ToRow Jockey

instance FromJSON Jockey where
  parseJSON = Aeson.genericParseJSON prefixOptions

instance ToJSON Jockey where
  toJSON     = Aeson.genericToJSON prefixOptions
  toEncoding = Aeson.genericToEncoding prefixOptions

instance Arbitrary Jockey where
  arbitrary = genericArbitrary uniform

instance ToSchema Jockey where
  declareNamedSchema _ = do
    textSchema   <- declareSchemaRef (Proxy :: Proxy T.Text)
    doubleSchema <- declareSchemaRef (Proxy :: Proxy Double)
    intSchema    <- declareSchemaRef (Proxy :: Proxy Int)
    boolSchema   <- declareSchemaRef (Proxy :: Proxy Bool)
    return
      $  NamedSchema (Just "Jockey")
      $  mempty
      &  type_
      ?~ SwaggerObject
      &  description
      ?~ "Jockey Entity"
      &  properties
      .~ fromList
           [ ("name"   , textSchema)
           , ("skill"  , doubleSchema)
           , ("age"    , intSchema)
           , ("deleted", boolSchema)
           ]

instance Crud Jockey where
  type DbKey Jockey = Int
  type DbConn Jockey = Connection
  insert conn Jockey {..} = do
    [Only jid] <- returning
      conn
      [sql|INSERT INTO JOCKEY (name, skill, age)
            VALUES (?, ?, ?)
            RETURNING id
      |]
      [(jockeyName, jockeySkill, jockeyAge)]
    return (Model jid Jockey { .. })

  findOne conn hid = do
    res <- query
      conn
      [sql|SELECT id, name, skill, age, deleted FROM JOCKEY WHERE id = ?|]
      (Only hid)
    return (listToMaybe res)

  findAll conn = query_
    conn
    "SELECT id, name, skill, age, deleted FROM JOCKEY WHERE deleted = FALSE"


  update conn (Model jid Jockey {..}) = do
    _ <- execute
      conn
      "UPDATE JOCKEY SET (name, skill, age) = (?, ?, ?) WHERE id = ?"
      (jockeyName, jockeySkill, jockeyAge, jid)
    return ()

  delete conn jid = do
    _ <- execute conn "UPDATE Jockey SET deleted = True WHERE id = ?" (Only jid)
    return undefined

runPostgresJockey
  :: Members '[Fail, Reader Connection, Embed IO] r
  => Sem (DbCrud Jockey : r) a
  -> Sem r a
runPostgresJockey = interpret $ \x -> do
  conn :: Connection <- ask
  case x of
    Insert Jockey {..} -> do
      [Only jid] <- embed $ returning
        conn
        [sql|INSERT INTO JOCKEY (name, skill, age)
              VALUES (?, ?, ?)
              RETURNING id
        |]
        [(jockeyName, jockeySkill, jockeyAge)]
      return (Model jid Jockey { .. })
    FindOne hid -> do
      res <- embed $ query
        conn
        [sql|SELECT id, name, skill, age, deleted FROM JOCKEY WHERE id = ?
        |]
        (Only hid)
      return (listToMaybe res)
    FindAll -> embed $ query_
      conn
      "SELECT id, name, skill, age, deleted FROM JOCKEY WHERE deleted = FALSE"

    Update (Model jid Jockey {..}) -> do
      _ <- embed $ execute
        conn
        "UPDATE JOCKEY SET (name, skill, age) = (?, ?, ?) WHERE id = ?"
        (jockeyName, jockeySkill, jockeyAge, jid)
      return ()
    Delete jid -> do
      _ <- embed $ execute conn
                           "UPDATE Jockey SET deleted = True WHERE id = ?"
                           (Only jid)
      return ()
