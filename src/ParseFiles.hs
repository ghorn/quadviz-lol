{-# OPTIONS_GHC -Wall #-}

module ParseFiles ( getStates
                  ) where

import qualified Data.Map as M

import SpatialMath
import Types

parseOneLine :: String -> [Double]
parseOneLine strs = read $ "[" ++ (replaceSpaces strs) ++ "]"
  where
    replaceSpaces :: String -> String
    replaceSpaces [] = []
    replaceSpaces (' ':xs) = ',':replaceSpaces xs
    replaceSpaces (x:xs)   = x:replaceSpaces xs

quadFromList :: (String -> [Double] -> Double) -> (String -> Double) -> [Double] -> QuadCopter
quadFromList getState getParam xs = QuadCopter xyz quat rotors
  where
    xyz = Xyz px py pz
    quat = Quat qs qx qy qz
    px  = getState "p_x" xs
    py  = getState "p_y" xs
    pz  = getState "p_z" xs
    qs = getState "q_3" xs
    qx = getState "q_0" xs
    qy = getState "q_1" xs
    qz = getState "q_2" xs
    getRotor k' = Rotor (Xyz x y z) (Xyz 0 0 1) mag
      where
        k = show k'
        x = getParam $ "rotors_p_"++k++"_0"
        y = getParam $ "rotors_p_"++k++"_1"
        z = getParam $ "rotors_p_"++k++"_2"
        mag = getState ("r_" ++ k) xs
    rotors = map getRotor [0..3::Int]

parseStates :: (String -> [Double] -> Double) -> (String -> Double) -> String -> [QuadCopter]
parseStates getState getParam raw = map ((quadFromList getState getParam) . parseOneLine) rawQuads
  where
    rawQuads = drop 1 $ lines raw -- first line is time

makeGetter :: String -> String -> [Double] -> Double
makeGetter raw = myLookup
  where
    names = lines raw
    myMap = M.fromList $ zip names [1..]
    myLookup name xs = case M.lookup name myMap of
      Nothing -> error $ "couldn't find name \""++name++"\" in map, keys:\n"++show (M.keys myMap)
      Just k -> if k < length xs then xs !! k else error "it's too long ;)"

getStates :: FilePath -> IO [QuadCopter]
getStates folder = do
  getParam <- getParamGetter folder
  getState <- fmap makeGetter $ readFile $ folder ++ "/states.labels"
  fmap (parseStates getState getParam) $ readFile $ folder ++ "/integration_states.txt"

dropEmpty :: [String] -> [String]
dropEmpty [] = []
dropEmpty ("":xs) = dropEmpty xs
dropEmpty (x:xs) = x:dropEmpty xs

getParamGetter :: FilePath -> IO (String -> Double)
getParamGetter folder = do
  keys   <- fmap (dropEmpty . lines) $ readFile $ folder ++ "/params.labels"
  values' <- fmap (dropEmpty . lines) $ readFile $ folder ++ "/integration_params.txt"
  let values = map read values'
      myMap = M.fromList (zip keys values)
      myLookup name = case M.lookup name myMap of
        Nothing -> error $ "couldn't find key \""++name++"\" in map, keys:\n"++show (M.keys myMap)
        Just x -> x
  return myLookup
