module Day3.Main where

import Prelude

import Data.Array (foldl, index)
import Data.Either (Either(..))
import Data.Foldable (or, sum)
import Data.Int (fromString)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (length)
import Data.String.Utils (lines, toCharArray)
import Data.Tuple (Tuple(..), fst, snd)
import Day3.Acc (Acc, addToValuesConditionally, moveToNextChar, moveToNextRow, updateCurrent)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (attempt, launchAff_, message)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

main :: Effect Unit
main = launchAff_ do
  result <- attempt $ readTextFile UTF8 "./inputs/day3_engine-schematic"
  case result of
    Left e ->
      liftEffect $ log $ "There was a problem with readTextFile: " <> message e
    Right engineSchematic ->
      liftEffect $ logShow $ getSumPartNumbers $ toMatrix engineSchematic

type Matrix = Array (Array String)
type Coords = Tuple Int Int

toMatrix :: String -> Matrix
toMatrix = map toCharArray <<< lines

isDigit :: String -> Boolean
isDigit x = isJust $ Int.fromString x

isPartNumber :: Matrix -> Coords -> Boolean
isPartNumber matrix coords = or do
  fstCoord <- shiftValues
  sndCoord <- shiftValues
  pure $ not (fstCoord == 0 && sndCoord == 0) &&
    case getValueAt $ Tuple (fst coords + fstCoord) (snd coords + sndCoord) of
      (Just ch) -> not (isDigit ch) && ch /= "."
      _ -> false
  where
  shiftValues = [ (-1), 0, 1 ]
  getValueAt newCoord =
    join $ flip index (snd newCoord) <$> index matrix (fst newCoord)

getSumPartNumbers :: Matrix -> Int
getSumPartNumbers matrix = (\result -> sum result.values)
  $ foldl processMatrix initAcc matrix
  where
  initAcc :: Acc
  initAcc =
    { values: []
    , current:
        { index: Tuple 0 0
        , value: ""
        , partNumber: false
        }
    }

  processMatrix :: Acc -> Array String -> Acc
  processMatrix acc row = (\accAfterRow -> moveToNextRow accAfterRow)
    $ foldl processRow acc row

  processRow :: Acc -> String -> Acc
  processRow acc ch = (\accAfterChar -> moveToNextChar accAfterChar) $
    if isDigit ch then updateCurrent (acc.current.value <> ch)
      ( acc.current.partNumber || isPartNumber matrix acc.current.index
      )
      acc
    else
      ( updateCurrent "" false
          <<< addToValuesConditionally
            ( length acc.current.value > 0 && acc.current.partNumber
            )
            [ fromMaybe 0 $ fromString acc.current.value ]
      ) acc
