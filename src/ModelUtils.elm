module ModelUtils exposing (..)

import Models exposing (..)

getColor : Tile -> Color
getColor tile =
  Tuple.first tile

getNumber : Tile -> Number
getNumber tile =
  Tuple.second tile

createTile : Color -> Number -> Tile
createTile color number = (color, number)

colorToInt : Color -> Int
colorToInt color =
  case color of
    Black -> 0
    Red -> 1
    Orange -> 2
    Blue -> 3

numberToInt : Number -> Int
numberToInt number =
  case number of
    One -> 1
    Two -> 2
    Three -> 3
    Four -> 4
    Five -> 5
    Six -> 6
    Seven -> 7
    Eight -> 8
    Nine -> 9
    Ten -> 10
    Eleven -> 11
    Twelve -> 12
    Thirteen -> 13

colorToString : Color -> String
colorToString color =
  case color of
    Black -> "Black"
    Red -> "Red"
    Orange -> "Orange"
    Blue -> "Blue"

numberToString : Number -> String
numberToString number =
  case number of
    One -> "One"
    Two -> "Two"
    Three -> "Three"
    Four -> "Four"
    Five -> "Five"
    Six -> "Six"
    Seven -> "Seven"
    Eight -> "Eight"
    Nine -> "Nine"
    Ten -> "Ten"
    Eleven -> "Eleven"
    Twelve -> "Twelve"
    Thirteen -> "Thirteen"

tileListToString : List Tile -> String
tileListToString tileList =
  "[" ++ (String.join ", " (List.map tileToString tileList)) ++ "]"

groupToString : Group -> String
groupToString group =
  tileListToString group

tileToString : Tile -> String
tileToString tile =
  let
    colorString = colorToString (getColor tile)
    numberString = numberToString (getNumber tile)
  in
  "(" ++ colorString ++ ", " ++ numberString ++ ")"

boardToString : Board -> String
boardToString board =
  "[" ++ (String.join ", " (List.map groupToString board)) ++ "]"

playerHandToString : PlayerHand -> String
playerHandToString playerHand =
  tileListToString playerHand
