{-# OPTIONS_GHC -Wall #-}

module Types ( Rotor(..)
             , QuadCopter(..)
             ) where

import SpatialMath

data Rotor = Rotor (Xyz Double) (Xyz Double) Double deriving Show
data QuadCopter = QuadCopter { qcPos :: Xyz Double
                             , qcRot :: Quat Double
                             , qcRotors :: [Rotor]
                             } deriving Show
