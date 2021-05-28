{-# LANGUAGE OverloadedStrings #-}

module HtmlReport where

import Data.Foldable (traverse_)
import Control.Monad (unless)
import Data.ByteString.Lazy (ByteString)
import Text.Blaze.Html5 as H (Html, string, text, ToValue(toValue), (!), body, docTypeHtml, h1,
                              head, i, img, style, title)
import Text.Blaze.Html5.Attributes (src)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Colonnade ( headed, Colonnade, Headed)
import Text.Blaze.Colonnade ( encodeHtmlTable)
import Fmt (pretty, Buildable)
import CovidData (CovidData(day, covidCasesConfirmed, requiringICUCovidCases, male, female, unknown, aged1To4, aged5To14, aged15To24, aged25To34, aged35To44, aged45To54, aged55To64, aged65up))
import StatReport(showPrice, StatEntry(cfield, meanVal, minVal, maxVal, daysBetweenMinMax))

viaFmt :: Buildable a => a -> Html
viaFmt = text . pretty

colStats :: Colonnade Headed StatEntry Html
colStats = mconcat
            [ headed "Covid Field" (i . string . show . cfield)
            , headed "Mean" (viaFmt . meanVal)
            , headed "Min" (viaFmt . minVal)
            , headed "Max" (viaFmt . maxVal)
            , headed "Days between Min/Max" (viaFmt . daysBetweenMinMax)
            ]

colData :: Colonnade Headed CovidData Html
colData = mconcat
            [ headed "StatisticsProfileDate" (viaFmt . day)
            , headed "CovidCasesConfirmed" (viaFmt . covidCasesConfirmed)
            , headed "RequiringICUCovidCases" (viaFmt . requiringICUCovidCases)
            , headed "Male" (viaFmt . male)
            , headed "Female" (viaFmt . female)
            , headed "Unknown" (viaFmt . unknown)
            , headed "Aged1to4" (viaFmt . aged1To4)
            , headed "Aged5to14" (viaFmt . aged5To14)
            , headed "Aged15to24" (viaFmt . aged15To24)
            , headed "Aged25to34" (viaFmt . aged25To34)
            , headed "Aged35to44" (viaFmt . aged35To44)
            , headed "Aged45to54" (viaFmt . aged45To54)
            , headed "Aged55to64" (viaFmt . aged55To64)
            , headed "Aged65up" (viaFmt . aged65up)
            ]

htmlReport :: (Functor t, Foldable t) => String -> t CovidData -> [StatEntry] -> [FilePath] -> ByteString 
htmlReport docTitle covid statEntries images = renderHtml $ docTypeHtml $ do
    H.head $ do
        title $ string docTitle
        style tableStyle
    body $ do
        unless (null images) $ do
            h1 "Charts"
            traverse_ ((img!).src.toValue) images

        h1 "Statistics Report Analysis for Covid Cases"
        encodeHtmlTable mempty colStats statEntries

        h1 "Covid Cases Data"
        encodeHtmlTable mempty colData covid
  where
      tableStyle = "table {border-collapse: collapse}" <>
           "td, th {border: 1px solid black; padding: 5px}"
