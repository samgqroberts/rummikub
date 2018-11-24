module Models exposing (..)

type Color
  = Black
  | Red
  | Orange
  | Blue

type Number
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Eleven
  | Twelve
  | Thirteen

type alias Tile = (Color, Number)

type alias Group = List Tile
type alias PlayerHand = List Tile

type alias GameState =
  { unflipped : List Tile
  , played : List Group
  , playerHands : List PlayerHand
  }