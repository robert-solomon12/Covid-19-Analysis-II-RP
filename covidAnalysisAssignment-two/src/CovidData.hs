{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}


module CovidData where

import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.ByteString.Char8 (unpack)
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), (.:), FromField(..))

data CovidData = CovidData {
                 day :: Day,
                 covidCasesConfirmed :: Int,
                 requiringICUCovidCases :: Int,
                 male :: Int,
                 female :: Int,
                 unknown :: Int,
                 aged1To4 :: Int,
                 aged5To14 :: Int,
                 aged15To24 :: Int,
                 aged25To34 :: Int,
                 aged35To44 :: Int,
                 aged45To54 :: Int,
                 aged55To64 :: Int,
                 aged65up :: Int
               }
    deriving (Generic)

instance FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%d/%m/%Y" . unpack

instance FromNamedRecord CovidData where
  parseNamedRecord m = CovidData <$> m .: "StatisticsProfileDate"
                                 <*> m .: "CovidCasesConfirmed"
                                 <*> m .: "RequiringICUCovidCases"
                                 <*> m .: "Male"
                                 <*> m .: "Female"
                                 <*> m .: "Unknown"
                                 <*> m .: "Aged1to4"
                                 <*> m .: "Aged5to14"
                                 <*> m .: "Aged15to24"
                                 <*> m .: "Aged25to34"
                                 <*> m .: "Aged35to44"
                                 <*> m .: "Aged45to54"
                                 <*> m .: "Aged55to64"
                                 <*> m .: "Aged65up"

data CField = CovidCasesConfirmed | RequiringICUCovidCases | Male | Female | Unknown | Aged1To4 | Aged5To14 | Aged15To24 | Aged25To34 | Aged35To44 | Aged45To54 | Aged55To64 | Aged65up
  deriving (Eq, Ord, Show, Enum, Bounded)



field2fun :: CField -> CovidData -> Double 
field2fun CovidCasesConfirmed = fromIntegral . covidCasesConfirmed
field2fun RequiringICUCovidCases = fromIntegral . requiringICUCovidCases
field2fun Male = fromIntegral . male
field2fun Female = fromIntegral . female
field2fun Unknown = fromIntegral . unknown
field2fun Aged1To4 = fromIntegral . aged1To4
field2fun Aged5To14 = fromIntegral . aged5To14
field2fun Aged15To24 = fromIntegral . aged15To24
field2fun Aged25To34 = fromIntegral . aged25To34
field2fun Aged35To44 = fromIntegral . aged35To44
field2fun Aged45To54 = fromIntegral . aged45To54
field2fun Aged55To64 = fromIntegral . aged55To64
field2fun Aged65up = fromIntegral . aged65up
