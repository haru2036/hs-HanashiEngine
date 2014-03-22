module Text.HanashiEngine.Types
( TrigramModel
)where
import Data.Map as Map

type TrigramModel = Map String (Map String (Map String Int))
