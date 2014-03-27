module Text.HanashiEngine.Train
( makeTrigramList
, makeTrigramModel
)where

import Data.Map as Map
import Data.List as List
import Text.HanashiEngine.Types

makeTrigramFreqList :: [String] -> [([String], Int)]
makeTrigramFreqList strings = getTrigramFrequency $ makeTrigramList strings

makeTrigramModel :: [([String], Int)] -> TrigramModel
makeTrigramModel [(a, count)] = makeTrigramItem (a, count)
makeTrigramModel listAll = Map.union (makeTrigramItem (head listAll)) (makeTrigramModel (tail listAll))

makeTrigramItem :: ([String], Int) -> TrigramModel
makeTrigramItem ([a, b, c], count) = singleton a (singleton b (singleton c count))

makeTrigramList :: [String] -> [[String]]
makeTrigramList [a, b, c] = [[a, b, c]]
makeTrigramList listAll = (take 3 listAll) : (makeTrigramList (tail listAll))

searchAndCountWords :: [String] -> [[String]] -> Int
searchAndCountWords key items = length $ List.filter (==key) items

getTrigramFrequency :: [[String]] -> [([String], Int)]
getTrigramFrequency sList = [(a, searchAndCountWords a sList) | a <- nub sList]

