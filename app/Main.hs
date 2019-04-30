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
      ( metavar "SOURCE" <> help "解析dat文件" )
  <*> flag False True
      ( long "pretty"
      <> short 'p'
      <> help "格式化" )
  <**> helper

cliBuilder :: ParserInfo CliOptions
cliBuilder = info optionBuilder $ fullDesc <> progDesc "解析好记星V1+学习机"

main :: IO ()
main = mainParse =<< execParser cliBuilder

mainParse (CliOptions source pretty) = parse source pretty
