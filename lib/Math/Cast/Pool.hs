{-|
Module       : Math.Cast.Pool
Description  : A module
Copyright    : None
License      : PublicDomain
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux-3.16

-}

module Math.Cast.Pool where

import Data.Ord (comparing)
import Math.Cast.Agent

-- |Pools are really just lists of agents
newtype Pool = Pool { agents :: [Agent] }
  deriving Eq

-- |Sum all the IQs in the pool
sumIqs :: Pool -> Double
sumIqs (Pool as) = sum $ map iq as
