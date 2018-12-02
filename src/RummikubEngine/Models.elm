module RummikubEngine.Models exposing (Board, Color(..), GameConfig, GameState, Group, Move(..), Number(..), PlayerHand, PlayerHands, PlayerState, PlayerStates, Tile, Unflipped)


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


type Move
    = Play Board
    | TakeTile
    | InitialPlay (List Group)


type alias GameConfig =
    { numPlayers : Int
    , tileDuplicates : Int -- how many (Black, One) tiles are there?
    , startingPlayerTileCount : Int
    }


type alias PlayerState =
    { hand : PlayerHand
    , hasPlayed : Bool
    }


type alias PlayerStates =
    List PlayerState


type alias GameState =
    { unflipped : Unflipped
    , board : Board
    , playerStates : PlayerStates
    , playerTurn : Int
    }
