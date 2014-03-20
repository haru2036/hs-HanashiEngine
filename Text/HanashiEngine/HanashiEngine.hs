--import Text.HanashiEngine.MeCabTools as MecabTools
--import Text.HanashiEngine.IO as HIO
import Data.Map
import Data.List

makeTrigramList :: [String] -> [[String]]
makeTrigramList all@[a, b, c] = [all]
makeTrigramList all = (take 3 all) : (makeTrigramList (tail all))

searchAndCountWords :: [String] -> [[String]] -> Int
searchAndCountWords key items = length $ Data.List.filter (==key) items

getTrigramFrequency :: [[String]] -> Map [String] Int
getTrigramFrequency sList = fromList [(a, searchAndCountWords a sList) | a <- nub sList]
