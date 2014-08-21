{-|
Module       : Math.Casting.Agent
Description  : A module
Copyright    : None
License      : PublicDomain
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux-3.16

-}

module Math.Casting.Agent where

import Data.Aeson
import Data.Ord (comparing)

-- |Agents have a genetic and environmental component
data Agent = Agent
    { geneticIq :: Double
    , envIq     :: Double
    }
  deriving (Eq, Show)

-- |Agents are ordered by their IQ
instance Ord Agent where
  compare = comparing iq

-- |Gets the IQ of an Agent
iq :: Agent -> Double
iq (Agent g e) = g + e

instance ToJSON Agent where
  toJSON (Agent g e) = object
    [ "genetic-iq" .= g
    , "env-iq" .= e
    ]
