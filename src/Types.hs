module Types where

import Data.Text
import Data.Aeson
import GHC.Generics
import Servant.Swagger.UI

data Horse = Horse
    { horseName :: Text
    , horseSpeed :: Double
    , horseImage :: Text
    , horseDeleted :: Bool
    }
    deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data Jockey = Jockey
    { jockeyName :: Text
    , jockeySkill :: Double
    , jockeyAge :: Int
    , jockeyDeleted :: Bool
    }
    deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data RaceEntry = RaceEntry
    { raceJockey :: Model Jockey
    , raceHorse :: Model Horse
    , raceId :: Int
    }
    deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data Model a = Model
    { modelId :: Int
    , model :: a
    }
    deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
