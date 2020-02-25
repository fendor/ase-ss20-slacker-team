module Race where

import           Horse
import           Jockey
import           Utils
import           Data.Aeson
import qualified Data.Aeson.Types              as Aeson
import           GHC.Generics
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.FromRow
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Instances.Text ( )
import           Generic.Random
import           Type.Reflection
import           Servant

data RaceEntry = RaceEntry
    { raceJockey :: Model Jockey
    , raceHorse :: Model Horse
    , raceId :: Int
    }
    deriving (Show, Eq, Read, Generic, Typeable)


type RaceApi =
       "race" :> Get '[JSON] [Int]
  :<|> "race" :> Capture "raceId" Int :> Get '[JSON] [Model RaceEntry]
  :<|> "race" :> Post '[JSON] (Int, [(Int, Int)])

instance FromJSON RaceEntry where
  parseJSON = Aeson.genericParseJSON prefixOptions

instance ToJSON RaceEntry where
  toJSON     = Aeson.genericToJSON prefixOptions
  toEncoding = Aeson.genericToEncoding prefixOptions

instance FromRow RaceEntry where
  fromRow = RaceEntry <$> fromRow <*> fromRow <*> field

instance Arbitrary RaceEntry where
  arbitrary = genericArbitrary uniform


findRace :: Connection -> Int -> IO [Model RaceEntry]
findRace conn rid = query
  conn
  [sql|
        SELECT r.id, j.id, j.name, j.skill, j.age, j.deleted, h.id, h.name, h.speed, h.image, h.deleted, r.race
        FROM RACEENTRY r
        JOIN JOCKEY j ON r.jockey = j.id JOIN HORSE h on h.id = r.horse
        WHERE r.race = ?
    |]
  (Only rid)

newRace :: Connection -> IO Int
newRace conn = do
  [Only rid] <- query_ conn "SELECT nextval('race_entry_seq')"
  return rid


insertRaceEntry :: Connection -> RaceEntry -> IO (Model RaceEntry)
insertRaceEntry conn RaceEntry {..} = do
  [Only rid] <- returning
    conn
    [sql|INSERT INTO RACEENTRY (jockey, horse, race)
             VALUES (?, ?, ?)
             RETURNING id
        |]
    [(modelId raceJockey, modelId raceHorse, raceId)]
  return (Model rid RaceEntry { .. })

createRace :: Connection -> Int -> [(Int, Int)] -> IO Int
createRace conn rid entries = do
  let packTuple :: (Int, Int) -> (Int, Int, Int)
      packTuple (jid, hid) = (jid, hid, rid)
  nums <- executeMany
    conn
    [sql|INSERT INTO RACEENTRY (jockey, horse, race)
             VALUES (?, ?, ?)
        |]
    (map packTuple entries)
  return $ fromIntegral nums
