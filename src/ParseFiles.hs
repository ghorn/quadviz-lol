{-# OPTIONS_GHC -Wall #-}

module ParseFiles ( getStates ) where

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

quadFromList :: (String -> [Double] -> Double) -> [Double] -> QuadCopter
quadFromList getter xs = QuadCopter xyz quat rotors
  where
    xyz = Xyz x y z
    quat = Quat qs qx qy qz
    x = getter "p_x" xs
    y = getter "p_y" xs
    z = getter "p_z" xs
    qs = getter "q_3" xs
    qx = getter "q_0" xs
    qy = getter "q_1" xs
    qz = getter "q_2" xs
    rotors = []

parseStates :: (String -> [Double] -> Double) -> String -> [QuadCopter]
parseStates getter raw = map ((quadFromList getter) . parseOneLine) rawQuads
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
  getter <- fmap makeGetter $ readFile $ folder ++ "/states.labels"
  fmap (parseStates getter) $ readFile $ folder ++ "/integration_states.txt"
