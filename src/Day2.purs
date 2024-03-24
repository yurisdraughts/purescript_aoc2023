module Day2 where

import Prelude

import Data.Array (foldl, index)
import Data.Array.NonEmpty (head, toArray)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Data.String.Regex (match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Aff (attempt, launchAff_, message)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

solve :: Effect Unit
solve = launchAff_ do
  result <- attempt $ readTextFile UTF8 "./inputs/games"
  case result of
    Left e ->
      liftEffect $ log $ "There was a problem with readTextFile: " <> message e
    Right games ->
      liftEffect $ logShow $ processInput games

type Cubes =
  { color :: String
  , number :: Int
  }

type Game =
  { id :: Int
  , subsets :: Array String
  }

testValue :: { red :: Int, green :: Int, blue :: Int }
testValue =
  { red: 12
  , green: 13
  , blue: 14
  }

parseCubes :: String -> Cubes
parseCubes cubeString = fromMaybe { color: "", number: 0 } do
  arrayMaybeString <- toArray <$> match re cubeString
  numberString <- index arrayMaybeString 1
  colorString <- index arrayMaybeString 2
  let number = fromMaybe 0 $ fromString $ fromMaybe "" numberString
  let color = fromMaybe "" $ colorString
  pure { number, color }
  where
  re = unsafeRegex """^(\d+) (red|green|blue)$""" noFlags

processCubeSubset :: String -> Boolean
processCubeSubset =
  foldl f true <<< split (Pattern ", ")
  where
  f false _ = false
  f _ cubeString =
    let
      cubes = parseCubes cubeString
    in
      cubes.color == "red" && cubes.number <= testValue.red
        || cubes.color == "green" && cubes.number <= testValue.green
        ||
          cubes.color == "blue" && cubes.number <= testValue.blue

parseGame :: String -> Game
parseGame gameString =
  let
    stringHalves = split (Pattern ": ") gameString
    idString = fromMaybe "" $ stringHalves `index` 0
    subsets = split (Pattern "; ") $ fromMaybe "" $ stringHalves `index` 1
    re = unsafeRegex """(\d+)""" noFlags
    id = fromMaybe 0 $ join $ map fromString $ join $ head <$> match re idString
  in
    { id, subsets }

processGame :: String -> Int
processGame gameString =
  let
    { id, subsets } = parseGame gameString
    f false _ = false
    f _ cubeSubset = processCubeSubset cubeSubset
  in
    if foldl f true subsets then id
    else 0

processInput :: String -> Int
processInput input = foldl f 0 $ lines input
  where
  f acc game = acc + processGame game