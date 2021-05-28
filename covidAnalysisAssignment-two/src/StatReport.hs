{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}




module StatReport where

import Data.Ord (comparing)
import Data.Foldable (minimumBy, maximumBy)
import Data.Time (diffDays)
import Fmt ( Buildable(..), Builder, (+|), (+||), pretty, (|+), (||+), fixedF )
import Colonnade ( ascii, headed )
import CovidData ( field2fun, CField(CovidCasesConfirmed), CField(RequiringICUCovidCases),
                   CField(Male), CField(Female), CField(Unknown), CField(Aged1To4), CField(Aged5To14), CField(Aged15To24), CField(Aged25To34), CField(Aged35To44), CField(Aged45To54),
                   CField(Aged55To64), CField(Aged65up), CovidData(day))


decimalPlacesFloating :: Int
decimalPlacesFloating = 2

data StatValue = StatValue {                   --this is used to format the calculated fields
    decimalPlaces :: Int,
    value :: Double 
}

data StatEntry = StatEntry{                    -- where we store our stat results
    cfield :: CField,
    meanVal :: StatValue,
    minVal :: StatValue,
    maxVal :: StatValue,
    daysBetweenMinMax :: Int 
}

mean :: (Fractional a, Foldable t) => t a -> a         --this allow us to calculate our mean for any foldable type
mean xs = sum xs / fromIntegral (length xs)


computeMinMaxDays :: (Ord a, Foldable t) => (CovidData -> a) -> t CovidData -> (a, a, Int)
computeMinMaxDays get covid = (get minQ, get maxQ, days)

  where
    cmp = comparing get
    minQ = minimumBy cmp covid
    maxQ = maximumBy cmp covid 
    days = fromIntegral $ abs $diffDays (day minQ) (day maxQ)

statInfo :: (Functor t, Foldable t) => t CovidData -> [StatEntry]
statInfo covid = fmap cFieldStatInfo [minBound .. maxBound]

  where
    decimalPlacesByCField CovidCasesConfirmed = 0
    decimalPlacesByCField RequiringICUCovidCases = 0
    decimalPlacesByCField Male = 0
    decimalPlacesByCField Female = 0
    decimalPlacesByCField Unknown = 0
    decimalPlacesByCField Aged1To4 = 0
    decimalPlacesByCField Aged5To14 = 0
    decimalPlacesByCField Aged15To24 = 0
    decimalPlacesByCField Aged25To34 = 0
    decimalPlacesByCField Aged35To44 = 0
    decimalPlacesByCField Aged45To54 = 0
    decimalPlacesByCField Aged55To64 = 0
    decimalPlacesByCField Aged65up = 0
    decimalPlacesByCField _ = decimalPlacesFloating

    cFieldStatInfo cfield = 
      let
        get = field2fun cfield
        (mn, mx, daysBetweenMinMax) = 
            computeMinMaxDays get covid
        decPlaces = decimalPlacesByCField cfield
        meanVal = StatValue decimalPlacesFloating
                            (mean $ fmap get covid)
        minVal = StatValue decPlaces mn
        maxVal = StatValue decPlaces mx
      in StatEntry {..}

instance Buildable StatValue where                                             -- we use instance to define the use of StatValue in statInfo
    build sv = fixedF (decimalPlaces sv) (value sv)

instance Buildable StatEntry where                                            -- we use instance to define how StatEntry will look. This structure will be used for text and html..   
    build StatEntry {..} =
        ""+||cfield||+": "
          +|meanVal|+" (mean), "
          +|minVal|+" (min), "
          +|maxVal|+" (max), "
          +|daysBetweenMinMax|+" (days)"

textReport :: [StatEntry] -> String 
textReport = ascii colStats
  where       
      colStats = mconcat
        [ headed "Covid Field" (show . cfield)
        , headed "Mean" (pretty . meanVal)
        , headed "Min" (pretty . minVal)
        , headed "Max" (pretty . maxVal)
        , headed "Days between Min/Max" (pretty . daysBetweenMinMax)
        ]

showPrice :: Double -> Builder                                                           -- we will use this for html formatting (auxiliary function)
showPrice = fixedF decimalPlacesFloating
