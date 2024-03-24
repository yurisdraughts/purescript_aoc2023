module Test.Main where

import Prelude

import Effect (Effect)
-- import Test.Day1 as Day1
-- import Test.Day2 as Day2
import Test.Day3.Main as Day3

main :: Effect Unit
main = do
  -- Day1.runTests
  -- Day2.runTests
  Day3.main
