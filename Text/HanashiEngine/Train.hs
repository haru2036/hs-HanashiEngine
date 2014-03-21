module Text.HanashiEngine.Train
( makeTrigramList
)where
--import Text.HanashiEngine.MeCabTools as MecabTools
--import Text.HanashiEngine.IO as HIO
import Data.Map as Map
import Data.List as List
type TrigramModel = Map String (Map String (Map String Int))

makeTrigramFreqList :: [String] -> [([String], Int)]
makeTrigramFreqList strings = getTrigramFrequency $ makeTrigramList strings

makeTrigramModel :: [([String], Int)] -> TrigramModel
makeTrigramModel [(a, count)] = makeTrigramItem (a, count)
makeTrigramModel listAll = Map.union (makeTrigramItem (head listAll)) (makeTrigramModel (tail listAll))

makeTrigramItem :: ([String], Int) -> TrigramModel
makeTrigramItem ([a, b, c], count) = singleton a (singleton b (singleton c count))

makeTrigramList :: [String] -> [[String]]
makeTrigramList listAll@[a, b, c] = [listAll]
makeTrigramList listAll = (take 3 listAll) : (makeTrigramList (tail listAll))

searchAndCountWords :: [String] -> [[String]] -> Int
searchAndCountWords key items = length $ List.filter (==key) items

getTrigramFrequency :: [[String]] -> [([String], Int)]
getTrigramFrequency sList = [(a, searchAndCountWords a sList) | a <- nub sList]

