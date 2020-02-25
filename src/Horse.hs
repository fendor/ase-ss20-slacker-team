module Horse where

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
import           Postgres.Simple               as Db
import           Utils
import           Postgres.Polysemy              ( DbCrud(..) )
import           Servant
import           Control.Monad.IO.Class         ( liftIO )


data Horse = Horse
    { horseName :: T.Text
    , horseSpeed :: Double
    , horseImage :: T.Text
    , horseDeleted :: Bool
    }
    deriving (Show, Eq, Read, Generic, Typeable)

type HorseApi = "horse"  :>
  (    Summary "Get all entities"  :> Get '[JSON] [Model Horse]
  :<|> Summary "Create new entity" :> ReqBody '[JSON] Horse :> Post '[JSON] (Model Horse)
  :<|> Summary "Update entity"     :> Capture "horseId" Int :> ReqBody '[JSON] Horse :> Patch '[JSON] ()
  :<|> Summary "Delete entity"     :> Capture "horseId" Int :> Delete '[JSON] ()
  :<|> Summary "Find entity by id" :> Capture "horseId" Int :> Get '[JSON] (Model Horse)
  )

horseApi :: Connection -> Server HorseApi
horseApi conn = findAll' :<|> create' :<|> update' :<|> delete' :<|> findOne'
 where
  findAll' :: Handler [Model Horse]
  findAll' = liftIO $ Db.findAll conn
  create' :: Horse -> Handler (Model Horse)
  create' horse = liftIO $ Db.insert conn horse
  update' :: Int -> Horse -> Handler ()
  update' hid horse = liftIO $ Db.update conn (Model hid horse)
  delete' :: Int -> Handler ()
  delete' hid = do
    (_ :: Horse) <- liftIO $ Db.delete conn hid
    return ()
  findOne' :: Int -> Handler (Model Horse)
  findOne' hid = liftIO (Db.findOne conn hid) >>= \case
    Nothing -> throwError err404
    Just h  -> return h

instance FromJSON Horse where
  parseJSON = Aeson.genericParseJSON prefixOptions

instance ToJSON Horse where
  toJSON     = Aeson.genericToJSON prefixOptions
  toEncoding = Aeson.genericToEncoding prefixOptions

instance FromRow Horse
instance ToRow Horse

instance ToSchema Horse where
  declareNamedSchema _ = do
    textSchema   <- declareSchemaRef (Proxy :: Proxy T.Text)
    doubleSchema <- declareSchemaRef (Proxy :: Proxy Double)
    boolSchema   <- declareSchemaRef (Proxy :: Proxy Bool)
    return
      $  NamedSchema (Just "Horse")
      $  mempty
      &  type_
      ?~ SwaggerObject
      &  description
      ?~ "Horse Entity"
      &  properties
      .~ fromList
           [ ("name"   , textSchema)
           , ("speed"  , doubleSchema)
           , ("image"  , textSchema)
           , ("deleted", boolSchema)
           ]

instance Arbitrary Horse where
  arbitrary = genericArbitrary uniform


instance Crud Horse where
  type DbKey Horse = Int
  type DbConn Horse = Connection
  insert conn Horse {..} = do
    [Only hid] <- returning
      conn
      [sql|INSERT INTO HORSE (name, speed, image)
            VALUES (?, ?, ?)
            RETURNING id
        |]
      [(horseName, horseSpeed, horseImage)]
    return (Model hid Horse { .. })

  findOne conn hid = do
    res <- query
      conn
      "SELECT id, name, speed, image, deleted FROM HORSE WHERE id = ?"
      (Only hid)
    return (listToMaybe res)

  findAll conn = query_
    conn
    "SELECT id, name, speed, image, deleted FROM HORSE WHERE deleted = FALSE"

  update conn (Model hid Horse {..}) = do
    _ <- execute
      conn
      "UPDATE Horse SET (name, speed, image) = (?, ?, ?) WHERE id = ?"
      (horseName, horseSpeed, horseImage, hid)
    return ()

  delete conn hid = do
    _ <- execute conn "UPDATE Horse SET deleted = True WHERE id = ?" (Only hid)
    return undefined

runPostgresHorse
  :: Members '[Fail, Reader Connection, Embed IO] r
  => Sem (DbCrud Horse : r) a
  -> Sem r a
runPostgresHorse = interpret $ \x -> do
  conn :: Connection <- ask
  case x of
    Insert Horse {..} -> do
      [Only hid] <- embed $ returning
        conn
        [sql|INSERT INTO HORSE (name, speed, image)
              VALUES (?, ?, ?)
              RETURNING id
        |]
        [(horseName, horseSpeed, horseImage)]
      return (Model hid Horse { .. })
    FindOne hid -> do
      res <- embed $ query
        conn
        "SELECT id, name, speed, image, deleted FROM HORSE WHERE id = ?"
        (Only hid)
      return (listToMaybe res)
    FindAll -> embed $ query_
      conn
      "SELECT id, name, speed, image, deleted FROM HORSE WHERE deleted = FALSE"

    Update (Model hid Horse {..}) -> do
      _ <- embed $ execute
        conn
        "UPDATE Horse SET (name, speed, image) = (?, ?, ?) WHERE id = ?"
        (horseName, horseSpeed, horseImage, hid)
      return ()
    Delete hid -> do
      _ <- embed
        $ execute conn "UPDATE Horse SET deleted = True WHERE id = ?" (Only hid)
      return ()
