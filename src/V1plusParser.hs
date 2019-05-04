{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module V1plusParser
    ( parse
    ) where

import Prelude as PRE hiding (readFile)
import OutputJson
import System.Process
import Control.Applicative
import Data.Bits (finiteBitSize)
import Data.ByteString (ByteString, readFile, hPutStr, hGetContents)
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Internal.Builder as TB
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Attoparsec.Text as PT
import qualified Data.Attoparsec.ByteString.Char8 as P8
import Data.ByteString.Lazy.UTF8 hiding (ByteString)
import Data.Aeson
import Data.Aeson.Encode.Pretty

parserError :: String -> String -> [String] -> String -> IO a
parserError prefix i es desc =
  let sep = "\n" in
  fail $ prefix <> " error: " <>
    i <> sep <> show es <> sep <> desc

gbk2utf8 :: ByteString -> IO String
gbk2utf8 s = do
  (Just stdin, Just stdout, _, _) <- createProcess (proc "iconv"
    ["-sc", "-f", "gb18030", "-t", "utf8"])
      { std_out = CreatePipe
      , std_in = CreatePipe }
  hPutStr stdin s
  r <- hGetContents stdout
  return $ toString $ BL.fromStrict r

convertedParser :: (Int, Int, Int) -> PT.Parser V1plusData
convertedParser (numberSize, articleSize, alartSize) = parser
  where
    parser :: PT.Parser V1plusData
    parser = do
      numbers <- numbersParser numberSize
      articles <- articlesParser articleSize
      alarts <- alartsParser alartSize
      return $ V1plusData numbers articles alarts

    stringnParser :: Int -> PT.Parser [String]
    stringnParser n = PT.count n $ PT.manyTill PT.anyChar $ PT.string "\NUL\NUL\NUL"

    numbersParser :: Int -> PT.Parser [V1plusNumber]
    numbersParser n = PT.count n numberParser

    numberParser :: PT.Parser V1plusNumber
    numberParser = do
      (name:phone:number:mail:company:address:postalCode:remark:_) <- stringnParser 8
      return $ V1plusNumber
        name phone number mail company address postalCode remark

    articlesParser :: Int -> PT.Parser [V1plusArticle]
    articlesParser n = PT.count n articleParser

    articleParser :: PT.Parser V1plusArticle
    articleParser = do
      (title:content:_) <- stringnParser 2
      return $ V1plusArticle
        title content

    alartsParser :: Int -> PT.Parser [V1plusAlart]
    alartsParser n = PT.count n alartParser

    alartParser :: PT.Parser V1plusAlart
    alartParser = do
      (state:at:date:time:content:_) <- stringnParser 5
      let state' = if state == "关" then V1plusAlartStateOff else V1plusAlartStateOn
      return $ V1plusAlart
        state' at date time content

convert :: [[[ByteString]]] -> IO V1plusData
convert sss = do
  s <- gbk2utf8 $ packs sss
  let s' = TL.toStrict $ TB.toLazyText $ TB.fromString s
  let maxs = (PRE.length $ sss!!0, PRE.length $ sss!!1, PRE.length $ sss!!2)
  case PT.parse (convertedParser maxs) s' of
    (PT.Done i v1plus) -> return v1plus
    (PT.Partial f) -> fail "convert parser error partial"
    (PT.Fail i es desc) -> parserError "convert parser" (T.unpack i) es desc
  where
    sep = "\NUL\NUL\NUL" :: ByteString
    packs :: [[[ByteString]]] -> ByteString
    packs sss = (join' sep . map (join' sep . map (join' sep))) sss <> sep
    join' :: ByteString -> [ByteString] -> ByteString
    join' = B.intercalate

v1plusParser :: P8.Parser (IO V1plusData)
v1plusParser = do
    P8.string "OZPCDATA\NUL"
    int32P -- equal int 0
    numberCount <- int32P
    numbers <- numbersP numberCount
    int32P -- equal int 1
    articleCount <- int32P
    articles <- articlesP articleCount
    int32P -- equal int 2
    alartCount <- int32P
    alarts <- alartsP alartCount
    return $ convert [numbers, articles, alarts]
  where
    binaryInt :: ByteString -> Int
    binaryInt s = getInt $ B.foldl add (1, 0) s
      where
        add :: (Int, Int) -> Char -> (Int, Int)
        add (base, result) c = (
            base * 256,
            result + base * ord c
          )
        getInt (_, result) = result

    int32P :: P8.Parser Int
    int32P = do
      int32bin <- P8.take 4
      return $ binaryInt int32bin

    stringP :: P8.Parser ByteString
    stringP = do
      size <- int32P
      s <- P8.take $ size - 1
      P8.anyChar
      return s

    numberParser :: P8.Parser [ByteString]
    numberParser = P8.count 8 stringP

    numbersP :: Int -> P8.Parser [[ByteString]]
    numbersP n = P8.count n numberParser

    articleParser :: P8.Parser [ByteString]
    articleParser = P8.count 2 stringP

    articlesP :: Int -> P8.Parser [[ByteString]]
    articlesP n = P8.count n articleParser

    alartParser :: P8.Parser [ByteString]
    alartParser = P8.count 5 stringP

    alartsP :: Int -> P8.Parser [[ByteString]]
    alartsP n = P8.count n alartParser

parse :: FilePath -> Bool -> IO ()
parse filepath isPretty = do
    cont <- readFile filepath
    case P8.parse v1plusParser cont of
      (PT.Done i v1plus) -> do
        v1plus' <- v1plus
        putStrLn $ toString $ encodeJSON v1plus'
        return ()
      (PT.Partial f) -> fail "parser error partial"
      (PT.Fail i es desc) -> parserError "parser" (B.unpack i) es desc
  where encodeJSON = if isPretty then encodePretty else encode
