module V1plusParser
    ( parse
    ) where

import OutputJson
import System.Process
import Data.List (isPrefixOf)
import Text.ParserCombinators.ReadP
import Data.ByteString.Lazy.UTF8
import Data.Aeson
import Data.Aeson.Encode.Pretty

notNul :: Char -> Bool
notNul = (/=) '\NUL'

notNulP :: ReadP String
notNulP = munch notNul

stringP :: ReadP String
stringP = do s <- notNulP; char '\NUL'; return s

rangeP :: Int -> Int -> ReadP Char -> ReadP String
rangeP min max parser = foldl1 (+++) $ map (`count` parser) [min..max]

blockP :: ReadP String
blockP = do
  ss <- count 2 sectionP
  return $ foldl1 (<>) ss

sectionP :: ReadP String
sectionP = do
  rand <- rangeP 0 2 (satisfy notNul)
  end <- string "\NUL\NUL" +++ string "\NUL\NUL\NUL"
  return $ rand <> end

sepBySectionP :: Int -> ReadP [String]
sepBySectionP n = count n $ do sectionP; stringP

exceptNextBlockP :: ReadP a -> ReadP a
exceptNextBlockP a = do
  s <- look
  if null $ readP_to_S blockP s
  then a
  else pfail

numberParser :: ReadP V1plusNumber
numberParser = do
  (name:number:_:_:_:address:_:remark:_) <- sepBySectionP 8
  return $ V1plusNumber name number address

numbersParser :: ReadP [V1plusNumber]
numbersParser = many $ exceptNextBlockP numberParser

articleParser :: ReadP V1plusArticle
articleParser = do
  (title:content:_) <- sepBySectionP 2
  return $ V1plusArticle title content

articlesParser :: ReadP [V1plusArticle]
articlesParser = many $ exceptNextBlockP articleParser

alartParser :: ReadP V1plusAlart
alartParser = do
  (state:at:date:time:content:_) <- sepBySectionP 5
  let state' = if state == "关" then V1plusAlartStateOff else V1plusAlartStateOn
  return $ V1plusAlart state' at date time content

alartsParser :: ReadP [V1plusAlart]
alartsParser = many alartParser

v1plusParser :: ReadP V1plusData
v1plusParser = do
  string "OZPCDATA\NUL\NUL\NUL\NUL\NUL\002\NUL\NUL\NUL"
  numbers <- numbersParser
  blockP
  articles <- articlesParser
  blockP
  alarts <- alartsParser
  return $ V1plusData numbers articles alarts

readAndIconvFile :: FilePath -> IO String
readAndIconvFile filepath = readProcess "iconv"
  ["-sc", filepath, "-f", "gb18030", "-t", "utf8"] ""

parse :: FilePath -> Bool -> IO ()
parse filepath isPretty = do
  cont <- readAndIconvFile filepath
  let s = readP_to_S v1plusParser cont
  putStrLn $ toString $ encodeJSON $ fst $ last s
  where encodeJSON = if isPretty then encodePretty else encode
