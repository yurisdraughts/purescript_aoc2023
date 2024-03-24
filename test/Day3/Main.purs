module Test.Day3.Main where

import Prelude

import Data.Tuple (Tuple(..))
import Day3.Main as Day3
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  suite "isPartNumber" do
    suite "true" do
      let matrix1 = [ [ "1", ".", "." ], [ "$", ".", "." ] ]
      test ("symbol below: " <> show matrix1) do
        Assert.equal true
          $ Day3.isPartNumber matrix1 (Tuple 0 0)
      let matrix2 = [ [ ".", "&", "." ], [ ".", "4", "." ] ]
      test ("symbol above: " <> show matrix2) do
        Assert.equal true
          $ Day3.isPartNumber matrix2 (Tuple 1 1)
      let matrix3 = [ [ ".", "7", "+" ], [ ".", ".", "." ] ]
      test ("symbol to the right: " <> show matrix3) do
        Assert.equal true
          $ Day3.isPartNumber matrix3 (Tuple 0 1)
      let matrix4 = [ [ ".", ".", "." ], [ "-", "0", "." ] ]
      test ("symbol to the left: " <> show matrix4) do
        Assert.equal true
          $ Day3.isPartNumber matrix4 (Tuple 1 1)
      let matrix5 = [ [ ".", "1", "." ], [ "@", ".", "." ] ]
      test ("symbol on the same diagonal: " <> show matrix5) do
        Assert.equal true
          $ Day3.isPartNumber matrix5 (Tuple 0 1)
      let matrix6 = [ [ ".", "@", "." ], [ ".", ".", "2" ] ]
      test ("symbol on the same diagonal (2): " <> show matrix6) do
        Assert.equal true
          $ Day3.isPartNumber matrix6 (Tuple 1 2)
    suite "false" do
      let matrix1 = [ [ ".", ".", "1" ], [ "$", ".", "." ] ]
      test ("no adjacent symbol: " <> show matrix1) do
        Assert.equal false
          $ Day3.isPartNumber matrix1 (Tuple 0 2)
      let matrix2 = [ [ ".", ".", "%" ], [ ".", ".", "." ], [ ".", ".", "0" ] ]
      test ("no adjacent symbol (2): " <> show matrix2) do
        Assert.equal false
          $ Day3.isPartNumber matrix2 (Tuple 2 2)
      let matrix3 = [ [ ")", ".", "." ], [ ".", ".", "." ], [ ".", "4", "." ] ]
      test ("no adjacent symbol (3): " <> show matrix3) do
        Assert.equal false
          $ Day3.isPartNumber matrix3 (Tuple 2 1)
  test "getSumPartNumbers" do
    let
      engineSchematic =
        """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""
    Assert.equal 4361 $ Day3.getSumPartNumbers $ Day3.toMatrix engineSchematic