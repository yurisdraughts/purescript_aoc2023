module Test.Day2
  ( runTests
  ) where

import Prelude

import Day2 as Day2
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

runTests :: Effect Unit
runTests = runTest do
  suite "Day 2" do
    suite "parseCubes" do
      test "correctly formatted input" do
        Assert.equal { color: "red", number: 3 } $ Day2.parseCubes "3 red"
        Assert.equal { color: "green", number: 1 } $ Day2.parseCubes "1 green"
        Assert.equal { color: "blue", number: 20 } $ Day2.parseCubes "20 blue"
      suite "wrong input" do
        let defaultCubes = { color: "", number: 0 }
        test "extra spaces" do
          Assert.equal defaultCubes $ Day2.parseCubes "3  red"
        test "no spaces" do
          Assert.equal defaultCubes $ Day2.parseCubes "20blue"
        test "no color" do
          Assert.equal defaultCubes $ Day2.parseCubes "15 "
        test "no number" do
          Assert.equal defaultCubes $ Day2.parseCubes " green"
    suite "processCubeSubset" do
      suite "correctly formatted input" do
        suite "results in true" do
          test "1 red, 2 green, 6 blue" do
            Assert.equal true $ Day2.processCubeSubset "1 red, 2 green, 6 blue"
          test "3 blue, 4 red" do
            Assert.equal true $ Day2.processCubeSubset "3 blue, 4 red"
          test "2 green" do
            Assert.equal true $ Day2.processCubeSubset "2 green"
        suite "results in false" do
          test "8 green, 6 blue, 20 red" do
            Assert.equal false
              $ Day2.processCubeSubset "8 green, 6 blue, 20 red"
          test "8 green, 16 blue" do
            Assert.equal false
              $ Day2.processCubeSubset "8 green, 16 blue"
          test "18 red, 1 green" do
            Assert.equal false
              $ Day2.processCubeSubset "18 red, 1 green"
      suite "wrong input results in false" do
        suite "semicolon between different colors" do
          test "1 red; 2 green, 6 blue" do
            Assert.equal false $ Day2.processCubeSubset "1 red; 2 green, 6 blue"
        suite "no space between different colors" do
          test "18 red,1 green" do
            Assert.equal false $ Day2.processCubeSubset "18 red,1 green"
        suite "extra spaces" do
          test "' 18 red, 1 green'" do
            Assert.equal false $ Day2.processCubeSubset "18 red,1 green"
          test "'18 red, 1 green '" do
            Assert.equal false $ Day2.processCubeSubset "18 red, 1 green "
    suite "parseGame" do
      suite "correct inputs" do
        let game1 = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
        let
          result1 =
            { id: 1
            , subsets: [ "3 blue, 4 red", "1 red, 2 green, 6 blue", "2 green" ]
            }
        let
          game2 =
            "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
        let
          result2 =
            { id: 3
            , subsets:
                [ "8 green, 6 blue, 20 red"
                , "5 blue, 4 red, 13 green"
                , "5 green, 1 red"
                ]
            }
        suite game1 do
          test game1 do
            Assert.equal result1 $ Day2.parseGame game1
        suite game2 do
          test game2 do
            Assert.equal result2 $ Day2.parseGame game2
      suite "wrong inputs result" do
        let game1 = "Game 1:3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
        let result1 = { id: 1, subsets: [ "" ] }
        let game2 = "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red;1 green, 1 blue"
        let
          result2 =
            { id: 2
            , subsets:
                [ "1 blue, 2 green"
                , "3 green, 4 blue, 1 red;1 green, 1 blue"
                ]
            }
        let game3 = "Game 3: 8 green, 6 blue, 20 red: 5 blue, 4 red, 13 green; 5 green, 1 red"
        let result3 = { id: 3, subsets: [ "8 green, 6 blue, 20 red" ] }
        let game4 = "Game : 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
        let
          result4 =
            { id: 0
            , subsets:
                [ "1 green, 3 red, 6 blue"
                , "3 green, 6 red"
                , "3 green, 15 blue, 14 red"
                ]
            }
        suite
          "no space after semicolon \
          \(id is correctly identified, but not subsets)"
          do
            test game1 do
              Assert.equal result1 $ Day2.parseGame game1
        suite
          "no space after colon \
          \(id and the first subset are correctly identified, \
          \but the remaining two subsets are not separated)"
          do
            test game2 do
              Assert.equal result2 $ Day2.parseGame game2
        suite
          "two semicolons \
          \(id and the first subset are correctly identified, \
          \but the rest are missing)"
          do
            test game3 do
              Assert.equal result3 $ Day2.parseGame game3
        suite "no game id" do
          test game4 do
            Assert.equal result4 $ Day2.parseGame game4
      suite "processInput" do
        test "correct input" do
          let
            input =
              """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""
          Assert.equal 8 $ Day2.processInput input
        suite "wrong input" do
          let
            input1 =
              """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 greenGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"""
          suite "first 3 games, no newline between 1st and 2nd" do
            test "produces 0, i.e. no valid games" do
              Assert.equal 0 $ Day2.processInput input1
          let
            input2 =
              """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blueGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"""
          suite "first 3 games, no newline between 2nd and 3rd" do
            test "produces 1, i.e. only the 1st game is valid" do
              Assert.equal 1 $ Day2.processInput input2