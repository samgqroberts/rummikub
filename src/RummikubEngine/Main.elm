module RummikubEngine.Main exposing (attemptMove, newGame)

import List
import List.Extra exposing (elemIndex, getAt, splitAt, uniqueBy)
import Random exposing (Seed)
import RummikubEngine.Models exposing (Board, Color(..), GameState, Group, Number(..), PlayerHand, Tile)
import RummikubEngine.Utils exposing (..)
import Tuple


newGame : Seed -> GameState
newGame seed =
    let
        ( unflipped1, playerHand1 ) =
            takeTiles seed allTiles startingPlayerTileCount

        ( unflipped2, playerHand2 ) =
            takeTiles seed unflipped1 startingPlayerTileCount

        ( unflipped3, playerHand3 ) =
            takeTiles seed unflipped2 startingPlayerTileCount

        ( unflipped, playerHand4 ) =
            takeTiles seed unflipped3 startingPlayerTileCount
    in
    { unflipped = unflipped
    , playerHands = [ playerHand1, playerHand2, playerHand3, playerHand4 ]
    , board = []
    , playerTurn = 0
    }


attemptMove : GameState -> Board -> Result String GameState
attemptMove current newBoard =
    let
        currentPlayerHand =
            defaultingToEmptyList (getAt current.playerTurn current.playerHands)

        newBoardTiles =
            List.concatMap (\a -> a) newBoard

        currentBoardTiles =
            List.concatMap (\a -> a) current.board

        ( playedTiles, _ ) =
            listDiff newBoardTiles currentBoardTiles
    in
    case containsAll currentPlayerHand playedTiles of
        False ->
            Err "Some played tiles are not in the player's hand"

        True ->
            case isValidBoard newBoard of
                False ->
                    Err "Some played groups are not valid"

                True ->
                    let
                        ( newCurrentPlayerHand, _ ) =
                            listDiff currentPlayerHand playedTiles

                        newPlayerHands =
                            replaceAt current.playerTurn newCurrentPlayerHand current.playerHands
                                -- TODO impossible state
                                |> Maybe.withDefault current.playerHands
                    in
                    Ok
                        { current
                            | board = newBoard
                            , playerHands = newPlayerHands
                            , playerTurn = nextPlayerTurn current
                        }
