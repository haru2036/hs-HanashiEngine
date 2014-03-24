module Text.HanashiEngine.Generate
( makeSentence
)where

import Text.HanashiEngine.Types

makeSentence :: TrigramModel -> [String] -> [String]
makeSentence model single@[a] = (makeBigram model a) : single
makeSentence model wipSentence = (makeTrigram model (take 2 wipSentence)) : wipSentence

makeTrigram :: TrigramModel -> [String] -> [String]

makeBigram :: TrigramModel -> String -> [String]

