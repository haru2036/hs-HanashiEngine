module Text.HanashiEngine.Generate
( makeSentence
)where

import Text.HanashiEngine.Types
import Data.Map as Map

makeSentence :: TrigramModel -> [String] -> [String]
makeSentence model single@[a] = (makeBigram model a) : single
makeSentence model wipSentence = (selectHeadWords (lookupHeadWords model (take 2 wipSentence)) {- randomValue -}) : wipSentence

lookupHeadWords :: TrigramModel -> [String] -> Maybe (Map String Int)
lookupHeadWords model [first, second] = (Map.lookup first model) >>= \x-> Map.lookup second x

selectHeadWords :: Map String Int -> Int -> String

makeBigram :: TrigramModel -> String -> [String]

