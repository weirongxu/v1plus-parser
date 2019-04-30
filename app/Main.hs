module Main where

import V1plusParser
import Options.Applicative
import Data.Semigroup ((<>))

data CliOptions = CliOptions
  { source :: String
  , pretty :: Bool }

optionBuilder :: Parser CliOptions
optionBuilder = CliOptions
  <$> strArgument
      ( metavar "SOURCE" <> help "需要解析的dat文件" )
  <*> flag False True
      ( long "pretty"
      <> short 'p'
      <> help "是否格式化" )
  <**> helper

cliBuilder :: ParserInfo CliOptions
cliBuilder = info optionBuilder $ fullDesc <> progDesc "好记星V1+学习机文件格式解析"

main :: IO ()
main = mainParse =<< execParser cliBuilder

mainParse (CliOptions source pretty) = parse source pretty
