module Generate
  ( generateContent
  ) where

import           Flipstone.Prelude
import qualified Generate.World as World

import qualified Data.Bool as B
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified System.Random as Random

generateContent :: Bool -> Bool -> Bool -> IO T.Text
generateContent shouldGenerateMission shouldGenerateEnviron shouldGenerateWorld = do
  gen <- Random.newStdGen
  pure
    $ T.intercalate "\n\n"
    $ catMaybes
        [ B.bool Nothing (Just $ generateMission gen) shouldGenerateMission
        , B.bool Nothing (Just $ generateEnviron gen) shouldGenerateEnviron
        , B.bool Nothing (Just $ generateWorld   gen) shouldGenerateWorld
        ]

generateMission :: Random.StdGen -> T.Text
generateMission _gen = "Mission: TODO"

generateEnviron :: Random.StdGen -> T.Text
generateEnviron _gen = "Environment: TODO"

generateWorld :: Random.StdGen -> T.Text
generateWorld gen =
  -- This stack of gens is kind of gross, but the only way I can figure to get
  -- each generated value to be distinct while maintaining purity.
  let (diameterGen, firstGen) = Random.split gen
      (gravityGen, secondGen) = Random.split firstGen
      (atmosphereAndColonyGen, thirdGen) = Random.split secondGen
      (dayGen, fourthGen) = Random.split thirdGen
      (yearGen, fifthGen) = Random.split fourthGen
      (landWaterGen, sixthGen) = Random.split fifthGen
      (continentGen, seventhGen) = Random.split sixthGen
      (biomeGen, _eighthGen) = Random.split seventhGen
   in T.unwords
        [ "This world has a diameter of " <> World.generateDiameter diameterGen
        , "and a gravitational pull " <> World.generateGravity gravityGen <> "."
        , World.generateAtmosphereAndColonizationLevel atmosphereAndColonyGen
        , "Its days are " <> World.generateDayLength dayGen <> " Earth-hours"
        , "long, and its years are " <> World.generateYearLength yearGen
        , "of its days. The water-to-land ratio of its surface is"
        , World.generateLandWaterRatio landWaterGen
        , "It has " <> World.generateContinents continentGen <> " continents,"
        , "averaging " <> World.generateBiomes biomeGen <> " biomes per"
        , "continent."
        ]
