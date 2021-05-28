{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Csv (decodeByName)
import Data.Foldable (toList)
import Data.Text (unpack)

import CovidData ( CovidData )
import Charts ( plotChart )
import StatReport ( statInfo, textReport )
import HtmlReport ( htmlReport )
import Params ( cmdLineParser, Params(..) )

generateReports :: (Functor t, Foldable t) =>
                   Params -> t CovidData -> IO ()
generateReports Params {..} covidStatsData = do
  unless silent $ putStr textRpt
  saveHtml htmlFile htmlRpt
  where
   statInfo' = statInfo covidStatsData
   textRpt = textReport statInfo'
   htmlRpt = htmlReport title covidStatsData statInfo' [chartFname | chart]
   
   chartFname = "chart1.svg" 
   title = "Covid Data"

   saveHtml Nothing _ = pure ()
   saveHtml (Just f) html = BL.writeFile f html


-- This function takes the constructed Params as an argument, reads and decodes the CSV file, and runs generateReports
work :: Params -> IO ()
work params = do
  csvData <- BL.readFile (fname params)
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, covidStatsData) -> generateReports params covidStatsData


 -- Responsible for running the command-line parser and delegates everything else to work
 -- (Uses monad bind >>= to take the output from cmdLineParser and push it through to work
main :: IO ()
main = cmdLineParser >>= work


readCovidStatsData :: FilePath -> IO [CovidData]
readCovidStatsData fpath = do
  csvData <- BL.readFile fpath
  case decodeByName csvData of
    Left err -> error err
    Right (_, covidStatsData) -> pure (toList covidStatsData)
