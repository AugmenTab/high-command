module Generate.Types
  ( Atmosphere(..), atmosphereToText
  , Colony, colonyToText
  , Dice(..), rollDice, rollDiceToText
  , Gravity(..), gravityToText
  ) where

import           Flipstone.Prelude

import qualified Data.List as L
import qualified Data.Text as T
import qualified System.Random as Random
import           Text.Show (show)

data Atmosphere
  = EarthLike
  | Methane
  | NoAtmosphere
  | HighOxygen
  | OtherAtmosphere
  deriving stock (Bounded, Enum)

instance Random.Random Atmosphere where
  randomR = defaultEnumRandomR
  random  = defaultBoundedRandom

atmosphereToText :: Atmosphere -> T.Text
atmosphereToText atmosphere =
  case atmosphere of
    EarthLike ->
      "Its atmosphere is oxygen- and nitrogen-based, like that of Earth."

    Methane ->
      T.unwords
        [ "Its atmosphere is methane-based, so only Grunts can properly breathe"
        , "in it without dying."
        ]

    NoAtmosphere ->
      "It has no usable atmosphere."

    HighOxygen ->
      T.unwords
        [ "Its atmosphere is similar to Earth-like atmospheres, but with a "
        , "higher oxygen content. Fires travel twice as fast and explosions"
        , "deal twice as much Base Damage on this planet."
        ]

    OtherAtmosphere ->
      T.unwords
        [ "Its atmosphere is atypical, based on some gas other than nitrogen,"
        , "oxygen, or methane. It is very poisonous to all playable lifeforms,"
        , "including most animals, unless otherwise specified."
        ]

data Colony
  = SmallOutpost
  | LargeOutpost
  | SmallSettlement
  | LargeSettlement
  | Settlements
  | Cities
  | Metropolises
  | Conurbation
  | Megalopolis
  | Ecumenopolis
  deriving stock (Bounded, Enum)

instance Random.Random Colony where
  randomR = defaultEnumRandomR
  random  = defaultBoundedRandom

colonyToText :: Atmosphere -> Colony -> T.Text
colonyToText NoAtmosphere _colony = "no colonies."
colonyToText _atmosphere  colony  =
  case colony of
    SmallOutpost    -> "a small outpost (10-100 population)."
    LargeOutpost    -> "a large outpost (100-1K population)."
    SmallSettlement -> "a small settlement (1K-10K population)."
    LargeSettlement -> "a large settlement (10K-100K population)."
    Settlements     -> "several settlements (100K-500K population)."
    Cities          -> "several cities (500K-2M population)."
    Metropolises    -> "several metropolises (2M-3M population)"
    Conurbation     -> "a conurbation (3M-10M population)."
    Megalopolis     -> "a megalopolis (10M-100M population)."
    Ecumenopolis    -> "an ecumenopolis (100+M population)."

data Dice
  = D5   Int
  | D10  Int
  | D100 Int

rollDice :: Dice -> Random.StdGen -> Int
rollDice (D5   dice) = sum . L.take dice . Random.randomRs (1, 5)
rollDice (D10  dice) = sum . L.take dice . Random.randomRs (1, 10)
rollDice (D100 dice) = sum . L.take dice . Random.randomRs (1, 100)

rollDiceToText :: Dice -> Random.StdGen -> T.Text
rollDiceToText dice = T.pack . show . rollDice dice

data Gravity
  = OneQuarterEarth
  | OneHalfEarth
  | EarthGravity
  | TwoTimesEarth
  | ThreeTimesEarth
  | FourTimesEarth
  | FiveTimesEarth
  | SixTimesEarth
  deriving stock (Bounded, Enum)

gravityToText :: Gravity -> T.Text
gravityToText gravity =
  let suffix txt = "roughly " <> txt <> " that of Earth's"
   in suffix $
        case gravity of
          OneQuarterEarth -> "¼ (one quarter)"
          OneHalfEarth    -> "½ (one half)"
          EarthGravity    -> "equivalent to"
          TwoTimesEarth   -> "twice"
          ThreeTimesEarth -> "three times"
          FourTimesEarth  -> "four times"
          FiveTimesEarth  -> "five times"
          SixTimesEarth   -> "six times"

-- Helpers
defaultBoundedRandom :: (Random.Random a, Bounded a, Random.RandomGen g)
                     => g -> (a, g)
defaultBoundedRandom = Random.randomR (minBound, maxBound)

defaultEnumRandomR :: (Enum a, Random.RandomGen g) => (a, a) -> g -> (a, g)
defaultEnumRandomR (lo, hi) gen =
  let (i, g) = Random.randomR (fromEnum lo, fromEnum hi) gen
   in (toEnum i, g)
