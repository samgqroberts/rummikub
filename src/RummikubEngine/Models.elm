module RummikubEngine.Models exposing (Board, Color(..), GameState, Group, Number(..), PlayerHand, PlayerHands, Tile, Unflipped)


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


type alias Tile =
    ( Color, Number )


type alias Group =
    List Tile


type alias PlayerHand =
    List Tile


type alias PlayerHands =
    List PlayerHand


type alias Board =
    List Group


type alias Unflipped =
    List Tile


type alias GameState =
    { unflipped : Unflipped
    , board : Board
    , playerHands : PlayerHands
    , playerTurn : Int
    }
