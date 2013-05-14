{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import Data.Maybe ( fromJust )
import SpatialMath
import qualified Vis

import Types ( QuadCopter(..), Rotor(..) )
import ParseFiles ( getStates )

data State = State [Maybe QuadCopter] [[Xyz Double]]

drawRotor :: Rotor -> Vis.VisObject Double
drawRotor (Rotor pos dir mag) = Vis.Trans pos $ Vis.VisObjects [disc, arrow]
  where
    disc = Vis.Cylinder (0.01,0.1) Vis.blue
    arrow = Vis.Arrow (0.0004*mag,5) dir Vis.green

drawOneQuad :: QuadCopter -> Vis.VisObject Double
drawOneQuad (QuadCopter r_n2b_n q_n2b rotors') = 
  Vis.Trans r_n2b_n $ Vis.RotQuat q_n2b $ Vis.VisObjects [box, axes, rotors]
  where
    box = Vis.Trans (Xyz 0 0 (-0.02)) $ Vis.Box (0.4,0.4,0.02) Vis.Solid Vis.red
    axes = Vis.Axes (0.5, 15)
    rotors = Vis.VisObjects $ map drawRotor rotors'

drawTrail :: [Xyz Double] -> Vis.VisObject Double
drawTrail positions = Vis.Line' $ zip positions colors
  where
    n = length positions - 1
    alphas :: [Float]
    alphas = reverse $ map (\x -> (fromIntegral x /fromIntegral n)) [0..n]
    colors :: [Vis.Color]
    colors = map (Vis.makeColor 0.1 0.8 1.0) alphas

drawFun :: State -> Vis.VisObject Double
drawFun (State [] _) = error "drawFun got empty list of quads :'("
drawFun (State (Nothing:_) _) = error "should never try to draw Nothing ;_;"
drawFun (State (Just quad:_) trails') =
  nwu2ned $ Vis.VisObjects [drawOneQuad quad, axes, trails, plane,txt]
  where
    nwu2ned = Vis.RotQuat $ Quat 0 1 0 0
    axes = Vis.Axes (0.5, 15)
    plane = Vis.Plane (Xyz 0 0 1) (Vis.makeColor 1 1 1 1) (Vis.makeColor 0.2 0.3 0.32 (realToFrac planeAlpha))
    planeAlpha = 0.1 :: Double
    trails = Vis.VisObjects $ map drawTrail trails'
    txt = Vis.VisObjects $
          zipWith (\s k -> Vis.Text2d s (30,fromIntegral $ 30*k) Vis.TimesRoman24 (Vis.makeColor 1 1 1 1)) messages (reverse [1..length messages])
    messages = zipWith f (qcRotors quad) [(0::Int)..]
      where
        f (Rotor _ _ mag) k= "rotor #"++show k++": " ++ show mag

getTrailPoints :: QuadCopter -> [Xyz Double]
getTrailPoints qc = map getOneTrailPoint (qcRotors qc)
  where
    getOneTrailPoint :: Rotor -> Xyz Double
    getOneTrailPoint (Rotor pos _ _) = (qcPos qc) + rotVecByQuatB2A (qcRot qc) pos

tryReset :: State -> State
tryReset (State (Nothing:quads) _) = State quads (initTrails (fromJust $ head quads))
tryReset x = x

simFun :: Float -> State -> State
simFun _ (State [] _) = error "no quads :'("
simFun _ (State (Nothing:_) _) = error "WHY OH WHY IS HEAD STATE NOTHING"
simFun _ (State (Just quad0:quads) trails) = tryReset $ State quads newtrails
  where
    newtrails = zipWith f (getTrailPoints quad0) trails
    f pos oldOnes = pos:(take 500 oldOnes)

initTrails :: QuadCopter -> [[Xyz Double]]
initTrails qc = map (\x -> [x]) $ getTrailPoints qc

main :: IO ()
main = do
  let folder = "/home/ghorn/quadviz/quadcopter_results"
  quads <- getStates folder
  let ts = 0.01 :: Double
      trails0 = initTrails (head quads)
      state0 = State (cycle ((map Just quads) ++ [Nothing])) trails0
  Vis.simulate (Just ((1260,940),(1930,40))) "joris is so nice" ts state0 drawFun simFun
