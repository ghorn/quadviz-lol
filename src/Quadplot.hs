{-# OPTIONS_GHC -Wall #-}
-- {-# Language DoAndIfThenElse #-}
-- {-# Language OverloadedStrings #-}
-- {-# Language CPP #-}

module Main ( main ) where

import SpatialMath
import qualified Vis

import Types ( QuadCopter(..) )
import ParseFiles ( getStates )

data State = State [QuadCopter]

drawOneQuad :: QuadCopter -> Vis.VisObject Double
drawOneQuad (QuadCopter r_n2b_n q_n2b rotors) = 
  Vis.Trans r_n2b_n $ Vis.RotQuat q_n2b $ Vis.VisObjects [box, axes]
  where
    box = Vis.Box (0.4,0.4,0.1) Vis.Solid Vis.red
    axes = Vis.Axes (0.5, 15)

drawFun :: State -> Vis.VisObject Double
drawFun (State []) = error "drawFun got empty list of quads :'("
drawFun (State (quad:_)) = nwu2ned $ Vis.VisObjects [drawOneQuad quad, axes, plane]
  where
    nwu2ned = Vis.RotQuat $ Quat 0 1 0 0
    axes = Vis.Axes (0.5, 15)
    plane = Vis.Plane (Xyz 0 0 1) (Vis.makeColor 1 1 1 1) (Vis.makeColor 0.2 0.3 0.32 (realToFrac planeAlpha))
    planeAlpha = 0.1 :: Double

simFun :: Float -> State -> State
simFun _ (State quads) = State (drop 1 quads)

main :: IO ()
main = do
  quads <- getStates "/home/ghorn/quadviz/quadcopter_results"
--  print (length integrationStates)
  let ts = 0.01 :: Double
      state0 = State (cycle quads)
  print (head quads)
  Vis.simulate (Just ((1260,940),(1930,40))) "joris is so nice" ts state0 drawFun simFun
