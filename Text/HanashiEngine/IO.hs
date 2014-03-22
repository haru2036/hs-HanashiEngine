module Text.HanashiEngine.IO
( loadPlainText 
, writeModelToFile
, readModelFromFile
)where

import System.IO
import Data.Serialize
import Data.ByteString
import Data.Either.Unwrap
import System.IO.UTF8 as S
import Text.HanashiEngine.Types

loadPlainText :: String -> IO String
loadPlainText fileName = do
  handle <- openFile fileName ReadMode
  contents <- S.hGetContents handle
  return $ contents

writeModelToFile :: String -> TrigramModel -> IO()
writeModelToFile filePath classes = do
  let bytes = Data.Serialize.encode classes
  Data.ByteString.writeFile filePath bytes

readModelFromFile :: String -> IO TrigramModel
readModelFromFile filePath = do
  bytes <- Data.ByteString.readFile filePath
  let decoded = Data.Serialize.decode bytes :: Either String TrigramModel
  return $ fromRight decoded

