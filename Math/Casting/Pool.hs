{-|
Module       : Math.Casting.Pool
Description  : A module
Copyright    : None
License      : PublicDomain
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux-3.16

-}

module Math.Casting.Pool where

import Data.Aeson
import Data.Ord (comparing)
import Math.Casting.Agent
import Math.Statistics

-- |Pools are really just lists of agents
newtype Pool = Pool { agents :: [Agent] }
  deriving (Eq, Show)

-- |Sum all the IQs in the pool
sumIqs :: Pool -> Double
sumIqs (Pool as) = sum $ map iq as

meanIq :: Pool -> Double
meanIq = mean . map iq . agents
poolMeanIq = meanIq

stdvIq :: Pool -> Double
stdvIq = stddev . map iq . agents
poolStdvIq = stdvIq

instance ToJSON Pool where
  toJSON (Pool as) = toJSON as
