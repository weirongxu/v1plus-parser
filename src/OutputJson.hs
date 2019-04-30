{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module OutputJson
    ( V1plusData (..)
    , V1plusNumber (..)
    , V1plusArticle (..)
    , V1plusAlart (..)
    , V1plusAlartState (..)
    ) where

import GHC.Generics
import Data.Aeson

data V1plusData = V1plusData
    { numbers :: [V1plusNumber]
    , articles :: [V1plusArticle]
    , alarts :: [V1plusAlart]
    } deriving (Generic, Show)

data V1plusNumber = V1plusNumber
    { name :: String
    , number :: String
    , address :: String
    } deriving (Generic, Show)

data V1plusArticle = V1plusArticle
    { title :: String
    , content :: String
    } deriving (Generic, Show)

data V1plusAlartState = V1plusAlartStateOn | V1plusAlartStateOff
  deriving (Generic, Show)

data V1plusAlart = V1plusAlart
    { state :: V1plusAlartState
    , at :: String
    , date :: String
    , time :: String
    , content' :: String
    } deriving (Generic, Show)

instance ToJSON V1plusNumber where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON V1plusNumber

instance ToJSON V1plusArticle where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON V1plusArticle

instance ToJSON V1plusAlartState where
  toJSON V1plusAlartStateOn = String "on"
  toJSON V1plusAlartStateOff = String "off"

instance FromJSON V1plusAlartState

instance ToJSON V1plusAlart where
  toJSON (V1plusAlart state at date time content') =
    object ["state" .= state , "at" .= at, "date" .= date, "time" .= time, "content" .= content']

instance FromJSON V1plusAlart

instance ToJSON V1plusData where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON V1plusData
