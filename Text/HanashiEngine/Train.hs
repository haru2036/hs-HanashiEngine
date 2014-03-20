module Text.HanashiEngine.Train
( makeTrigramList
)where
--import Text.HanashiEngine.MeCabTools as MecabTools
--import Text.HanashiEngine.IO as HIO
import Data.Map
import Data.List
makeTrigramFreqList :: [String] -> Map [String] Int
makeTrigramFreqList strings = getTrigramFrequency $ makeTrigramList strings

makeTrigramList :: [String] -> [[String]]
makeTrigramList listAll@[a, b, c] = [listAll]
makeTrigramList listAll = (take 3 listAll) : (makeTrigramList (tail listAll))

searchAndCountWords :: [String] -> [[String]] -> Int
searchAndCountWords key items = length $ Data.List.filter (==key) items

getTrigramFrequency :: [[String]] -> Map [String] Int
getTrigramFrequency sList = fromList [(a, searchAndCountWords a sList) | a <- nub sList]

