--import Text.HanashiEngine.MeCabTools as MecabTools
--import Text.HanashiEngine.IO as HIO

makeTrigramList :: [String] -> [[String]]
makeTrigramList all@[a, b, c] = [all]
makeTrigramList all = (take 3 all) : (makeTrigramList (tail all))
