module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Day1 (calculateCalibrationSum)

main :: Effect Unit
main = do
  logShow $ calculateCalibrationSum calibrationDocument

calibrationDocument :: String
calibrationDocument = """
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
"""