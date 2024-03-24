module Test.Day1
  ( runTests
  ) where

import Prelude

import Day1 as Day1
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

calibrationDocument :: String
calibrationDocument =
  """
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
"""

runTests :: Effect Unit
runTests = runTest do
  suite "Day 1" do
    test "digitsFromLine" do
      Assert.equal "12" $ Day1.digitsFromLine "1abc2"
      Assert.equal "38" $ Day1.digitsFromLine "pqr3stu8vwx"
      Assert.equal "12345" $ Day1.digitsFromLine "a1b2c3d4e5f"
      Assert.equal "7" $ Day1.digitsFromLine "treb7uchet"
    test "calibrationValueFromDigits" do
      Assert.equal 12 $ Day1.calibrationValueFromDigits "12"
      Assert.equal 38 $ Day1.calibrationValueFromDigits "38"
      Assert.equal 15 $ Day1.calibrationValueFromDigits "12345"
      Assert.equal 77 $ Day1.calibrationValueFromDigits "7"
    test "calculateCalibrationSum" do
      Assert.equal 142 $ Day1.calculateCalibrationSum calibrationDocument