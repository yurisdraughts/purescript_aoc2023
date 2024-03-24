module Day3.Acc where

import Prelude

import Data.Tuple (Tuple(..), fst)
import Debug (spy)

type CurrentNumber =
  { index :: Tuple Int Int
  , value :: String
  , partNumber :: Boolean
  }

type Acc =
  { values :: Array Int
  , current :: CurrentNumber
  }

moveToNextChar :: Acc -> Acc
moveToNextChar acc = acc { current { index = (_ + 1) <$> acc.current.index } }

moveToNextRow :: Acc -> Acc
moveToNextRow acc = acc { current { index = Tuple (fst acc.current.index + 1) 0 } }

updateCurrent :: String -> Boolean -> Acc -> Acc
updateCurrent value partNumber acc = acc
  { current
      { value = value
      , partNumber = partNumber
      }
  }

addToValuesConditionally :: Boolean -> Array Int -> Acc -> Acc
addToValuesConditionally cond values acc =
  if cond then acc { values = acc.values <> values }
  else acc
