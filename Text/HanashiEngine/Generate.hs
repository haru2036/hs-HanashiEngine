module Text.HanashiEngine.Generate
( makeSentence
)where

import System.Random
import Text.HanashiEngine.Types
import Data.Map as Map

makeSentence :: TrigramModel -> [String] -> [Double] -> [String]
makeSentence model single@[a] randomList = (makeBigram model a) : single
makeSentence model wipSentence randomList = makeSentence model ((selectHeadWords (lookupHeadWords model (take 2 wipSentence)) (head randomList)) : wipSentence) (drop 1 randomList)

lookupHeadWords :: TrigramModel -> [String] -> Maybe (Map String Int)
lookupHeadWords model [first, second] = (Map.lookup first model) >>= \x-> Map.lookup second x

selectHeadWords :: Map String Int -> Double -> String
selectHeadWords modelHead randomValue = (calcRandomPosition modelHead randomValue)

calcRandomPosition :: [(String, Int)] -> Double -> Double
calcRandomPosition = (1.0 / (sum (snd (unzip modelHead)))) * randomValue

lookupWordOfRandomPosition :: [(String, Int)] -> Int -> Double -> String
lookupWordOfRandomPosition modelHead sum position 
  | sum < position = lookupWordOfRandomPosition (drop 1 modelHead) (sum + (snd (head modelHead))) position
  | sum >= position = fst (head modelHead)

makeBigram :: TrigramModel -> String -> [String]

makeInfiniteRandoms :: IO [Double]
makeInfiniteRandoms = do
  gen <- getStdGen 
  return $ randoms gen
