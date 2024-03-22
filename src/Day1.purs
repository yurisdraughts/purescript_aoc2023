module Day1 where

import Prelude

import Data.Array (filter)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.String.CodeUnits (take, takeRight)
import Data.String.Utils (lines, toCharArray)
import Effect (Effect)
import Effect.Aff (attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Exception (message)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

solve :: Effect Unit
solve = launchAff_ do
  result <- attempt $ readTextFile UTF8 "./inputs/calibrationValues"
  case result of
    Left e ->
      liftEffect $ log $ "There was a problem with readTextFile: " <> message e
    Right calibrationDocument ->
      liftEffect $ logShow $ calculateCalibrationSum calibrationDocument

calculateCalibrationSum :: String -> Int
calculateCalibrationSum calibrationDocument = sum
    $ map (calibrationValueFromDigits <<< digitsFromLine)
    $ lines calibrationDocument

digitsFromLine :: String -> String
digitsFromLine = joinWith ""
  <<< filter (\str -> Int.fromString str /= Nothing)
  <<< toCharArray

calibrationValueFromDigits :: String -> Int
calibrationValueFromDigits str = fromMaybe 0
  $ Int.fromString
  $ take 1 str <> takeRight 1 str
