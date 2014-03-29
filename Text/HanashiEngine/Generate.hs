module Text.HanashiEngine.Generate
( makeSentence
)where

import System.Random
import Text.HanashiEngine.Types
import Data.Map as Map

makeSentence :: TrigramModel -> [String] -> [Double] -> [String] 
makeSentence model single@[a] randomList = makeSentence model ((makeBigram model a (head randomList)) ++ single) (drop 1 randomList)
makeSentence model wipSentence randomList 
  |"\n" /=  (head wipSentence) = makeSentence model ( maybe ("\n") (\x->x) ((lookupHeadWords model (take 2 wipSentence))  >>= (\x->return (selectHeadWords (toList x) (head randomList)))) : wipSentence) (drop 1 randomList)
  |otherwise = wipSentence

lookupHeadWords :: TrigramModel -> [String] -> Maybe (Map String Int)
lookupHeadWords model [first, second] = (Map.lookup first model) >>= \x-> Map.lookup second x

selectHeadWords :: [(String, Int)] -> Double -> String
selectHeadWords modelHead randomValue = lookupWordOfRandomPosition modelHead (truncate (calcModelHeadToRandomPosition modelHead randomValue))

calcModelHeadToRandomPosition :: [(String, Int)] -> Double -> Double
calcModelHeadToRandomPosition modelHead randomValue = calcRandomPosition (sum (snd (unzip modelHead))) randomValue

calcRandomPosition :: Int -> Double -> Double
calcRandomPosition lengthSum randomValue = (1.0 / (fromIntegral lengthSum)) * randomValue

lookupWordOfRandomPosition :: [(String, Int)] -> Int -> String
lookupWordOfRandomPosition modelHead sum = lookupWordOfRandomPositionRec modelHead sum 0

lookupWordOfRandomPositionRec :: [(String, Int)] -> Int -> Int -> String
lookupWordOfRandomPositionRec modelHead sum position 
  | sum < position = lookupWordOfRandomPositionRec (drop 1 modelHead) (sum + (snd (head modelHead))) position
  | sum >= position = fst (head modelHead)

makeBigram :: TrigramModel -> String -> Double -> [String]
makeBigram model startWord randomValue = let 
                                          keysList = keys $ Map.lookup startWord model 
                                         in 
                                           (truncate (calcRandomPosition (length keysList))) !! keysList

makeInfiniteRandoms :: IO [Double]
makeInfiniteRandoms = do
  gen <- getStdGen 
  return $ randoms gen
