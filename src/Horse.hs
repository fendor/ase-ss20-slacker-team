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
import           Polysemy.Error
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
import           Postgres.Polysemy              ( DbCrud(..) )
import qualified Postgres.Polysemy as P
import           Servant

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

horseApi :: Members '[DbCrud Horse, Error T.Text] r => ServerT HorseApi (Sem r)
horseApi =
  P.findAll :<|> P.insert :<|> update' :<|> P.delete @Horse :<|> findOne'
 where
  update' :: Member (DbCrud Horse) r => Int -> Horse -> Sem r ()
  update' hid horse = P.update (Model hid horse)

  findOne'
    :: Members '[Error T.Text, DbCrud Horse] r => Int -> Sem r (Model Horse)
  findOne' hid = do
    P.findOne hid >>= \case
      Nothing -> throw @T.Text ""
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
