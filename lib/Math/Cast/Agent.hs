{-|
Module       : Math.Cast.Agent
Description  : A module
Copyright    : None
License      : PublicDomain
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux-3.16

-}

module Math.Cast.Agent where

import Data.Ord (comparing)

-- |Agents have a genetic and environmental component
data Agent = Agent
    { geneticIq :: Double
    , envIq     :: Double
    }
  deriving Eq

-- |Agents are ordered by their IQ
instance Ord Agent where
  compare = comparing iq

-- |Gets the IQ of an Agent
iq :: Agent -> Double
iq (Agent g e) = g + e
