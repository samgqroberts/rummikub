module RummikubEngine.Main exposing (attemptMove, newGame)

import List
import List.Extra exposing (elemIndex, getAt, splitAt, uniqueBy)
import Random exposing (Seed)
import RummikubEngine.Models exposing (Board, Color(..), GameState, Group, Number(..), PlayerHand, Tile)
import RummikubEngine.Utils exposing (..)
import Tuple


newGame : Seed -> Int -> GameState
newGame seed numPlayers =
    let
        ( unflipped, playerHands ) =
            List.foldl
                (\_ ( currUnflipped, currPlayerHands ) ->
                    let
                        ( newUnflipped, newPlayerHand ) =
                            takeTiles seed currUnflipped defaultStartingPlayerTileCount
                    in
                    ( newUnflipped, newPlayerHand :: currPlayerHands )
                )
                ( allTiles defaultTileDuplicates, [] )
                (List.range 0 (numPlayers - 1))
    in
    { unflipped = unflipped
    , playerHands = playerHands
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
