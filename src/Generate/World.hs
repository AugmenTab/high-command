{-# LANGUAGE ScopedTypeVariables #-}
module Generate.World
  ( generateAtmosphereAndColonizationLevel
  , generateBiomes
  , generateContinents
  , generateDayLength
  , generateDiameter
  , generateGravity
  , generateLandWaterRatio
  , generateYearLength
  ) where

import           Flipstone.Prelude
import qualified Generate.Types as Types

import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified Data.Text as T
import           Data.Tuple (fst)
import qualified System.Random as Random
import           Text.Show (show)

generateAtmosphereAndColonizationLevel :: Random.StdGen -> T.Text
generateAtmosphereAndColonizationLevel gen =
  let (atmosphere :: Types.Atmosphere, nxtGen) = Random.random gen
   in T.unwords
        [ Types.atmosphereToText atmosphere
        , "It hosts"
        , Types.colonyToText atmosphere $ fst $ Random.random nxtGen
        ]

generateBiomes :: Random.StdGen -> T.Text
generateBiomes = Types.rollDiceToText (Types.D10 1)

generateContinents :: Random.StdGen -> T.Text
generateContinents = T.pack . show . (+) 3 . Types.rollDice (Types.D10 1)

generateDayLength :: Random.StdGen -> T.Text
generateDayLength gen =
  let (diceGen, indexGen) = Random.split gen
   in generateTimespan (indexD10 indexGen)
        $ [ 5  + Types.rollDice (Types.D5  1)  diceGen
          , 10 + Types.rollDice (Types.D5  1)  diceGen
          , 10 + Types.rollDice (Types.D10 1)  diceGen
          , 10 + Types.rollDice (Types.D10 2)  diceGen
          , 15 + Types.rollDice (Types.D10 3)  diceGen
          , 20 + Types.rollDice (Types.D10 4)  diceGen
          , 25 + Types.rollDice (Types.D10 5)  diceGen
          , 30 + Types.rollDice (Types.D10 6)  diceGen
          , 35 + Types.rollDice (Types.D10 7)  diceGen
          , 50 + Types.rollDice (Types.D10 10) diceGen
          ]

generateDiameter :: Random.StdGen -> T.Text
generateDiameter gen =
  (<> " kilometers")
    $ flip (L.!!) (indexD10 gen)
    $ [ "3,000"
      , "6,000"
      , "8,000"
      , "10,000"
      , "12,700"
      , "14,000"
      , "15,000"
      , "17,000"
      , "18,000"
      , "20,000"
      ]

generateGravity :: Random.StdGen -> T.Text
generateGravity gen =
  Types.gravityToText
    $ flip (L.!!) (indexD10 gen)
    $ L.cons Types.EarthGravity
    $ L.cons Types.EarthGravity
    $ ([ minBound..maxBound ] :: [Types.Gravity])

generateLandWaterRatio :: Random.StdGen -> T.Text
generateLandWaterRatio gen =
  let water = (+) 30 $ (*) 5 $ Types.rollDice (Types.D10 1) gen
   in T.unwords
        [ T.pack (show water) <> "% water,"
        , T.pack (show $ 100 - water) <> "% land."
        ]

generateYearLength :: Random.StdGen -> T.Text
generateYearLength gen =
  let (diceGen, indexGen) = Random.split gen
   in generateTimespan (indexD10 indexGen)
        $ [ 50 + Types.rollDice (Types.D100 1) diceGen
          , 75 + Types.rollDice (Types.D100 1) diceGen
          , 50 + Types.rollDice (Types.D100 2) diceGen
          , 75 + Types.rollDice (Types.D100 2) diceGen
          , 50 + Types.rollDice (Types.D100 3) diceGen
          , 75 + Types.rollDice (Types.D100 3) diceGen
          , 50 + Types.rollDice (Types.D100 4) diceGen
          , 75 + Types.rollDice (Types.D100 4) diceGen
          , 50 + Types.rollDice (Types.D100 5) diceGen
          , 75 + Types.rollDice (Types.D100 5) diceGen
          ]

-- Helpers
generateTimespan :: Int -> [Int] -> T.Text
generateTimespan index =
  T.pack
    . show
    . flip (L.!!) index
    . mconcat
    . L.zipWith (\fn calc -> fn calc) timeSpread

indexD10 :: Random.StdGen -> Int
indexD10 gen = Types.rollDice (Types.D10 1) gen - 1

timeSpread :: [a -> [a]]
timeSpread =
  [ L.replicate 7
  , L.replicate 9
  , L.replicate 9
  , L.replicate 15
  ] <> L.replicate 6 (L.replicate 10)
