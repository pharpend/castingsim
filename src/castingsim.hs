{-|
Module       : Main
Description  : Runs the casting SimulationConf
Copyright    : None
License      : PublicDomain
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux-3.16

-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Random
import           Data.Random.Distribution.Normal

-- Import our modules
import           Math.Casting.Agent
import           Math.Casting.Pool

data SimulationConf = SimulationConf
    { numberOfRuns :: Int
    , meanIq       :: Double
    , stdvIq       :: Double
    , heritability :: Double
    , poolSizes    :: [Int]
    }
  deriving (Show, Eq)

defaultSimulationConf :: SimulationConf
defaultSimulationConf = SimulationConf 5 100 15 0.5 (replicate 5 1000)

instance FromJSON SimulationConf where
  parseJSON (Object v) = SimulationConf
    <$> v .:? "number-of-runs" .!= 5
    <*> v .:? "mean-iq"        .!= 100
    <*> v .:? "stdv-iq"        .!= 15
    <*> v .:? "heritability"   .!= 0.5
    <*> v .:? "pool-sizes"     .!= replicate 5 1000
  parseJSON _ = fail "Not an object"

data PoolGenerate = PoolGenerate
    { poolSize    :: Int
    , geneticMean :: Double
    , geneticStdv :: Double
    , envMean     :: Double
    , envStdv     :: Double
    }
  deriving (Show, Eq)

defaultGenerate :: PoolGenerate
defaultGenerate = (!! 0) $ mkGenerate defaultSimulationConf

generatePool :: PoolGenerate -> IO Pool
generatePool pg = Pool <$> agents
  where agents = replicateM (poolSize pg) $ generateAgent pg
  
generateAgent :: PoolGenerate -> IO Agent
generateAgent g = Agent <$> giq <*> eiq
  where
    giq = runRVar (normal (geneticMean g) (geneticStdv g)) StdRandom
    eiq = runRVar (normal (envMean g) (envStdv g)) StdRandom

mkGenerate :: SimulationConf -> [PoolGenerate]
mkGenerate (SimulationConf _ totalMean totalStdv h sizes) = map mkPG sizes
  where
    mkPG :: Int -> PoolGenerate
    mkPG size = PoolGenerate size gmu gsig emu esig
    -- Total variance
    totalVar = totalStdv ** 2
    -- Genetic mean
    gmu = h * totalMean
    -- Environmental mean
    emu = (1 - h) * totalMean
    -- Genetic std
    gsig = sqrt $ h * totalVar
    -- Genetic std
    esig = sqrt $ (1 - h) * totalVar
  
main :: IO ()
main = return ()
