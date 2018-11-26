module Main exposing (..)

import Browser
import Random
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
import Models exposing (GameState, Unflipped, Board, PlayerHand, PlayerHands, Group)
import ModelUtils exposing (..)
import Game exposing (newGame)

---- MODEL ----

type alias Model =
  { gameState: GameState
  }

initialState =
  { gameState = newGame <| Random.initialSeed 10 {- TODO randomize seed -}
  }

init : ( Model, Cmd Msg )
init =
  ( initialState, Cmd.none )

---- UPDATE ----

type Msg
  = NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( model, Cmd.none )

---- VIEW ----

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Rummikub" ]
    , div []
      [ viewUnflipped model.gameState.unflipped
      , viewBoard model.gameState.board
      , viewPlayerHands model.gameState.playerHands
      ]
    ]

viewUnflipped : Unflipped -> Html Msg
viewUnflipped unflipped =
  div [] [ text ("Number of Unflipped Tiles: " ++ (String.fromInt <| List.length unflipped))]

viewBoard : Board -> Html Msg
viewBoard board =
  div [] [ text <| "Board: " ++ (boardToString board) ]

playerIndexToName : Int -> String
playerIndexToName index =
  "Player " ++ (String.fromInt <| index + 1)

viewPlayerHands : PlayerHands -> Html Msg
viewPlayerHands playerHands =
  div [] (
    playerHands
      |> List.indexedMap Tuple.pair
      |> List.map (Tuple.mapFirst playerIndexToName)
      |> List.map viewPlayerHand
  )

viewPlayerHand : (String, PlayerHand) -> Html Msg
viewPlayerHand nameAndHand =
  div [] [ text <| (Tuple.first nameAndHand) ++ "'s Hand: " ++ (playerHandToString <| Tuple.second nameAndHand) ]

---- PROGRAM ----

main : Program () Model Msg
main =
  Browser.element
    { view = view
    , init = \_ -> init
    , update = update
    , subscriptions = always Sub.none
    }
