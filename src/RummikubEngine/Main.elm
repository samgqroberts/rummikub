module RummikubEngine.Main exposing (attemptMove, newGame, newGameWithDefaults)

import List
import List.Extra exposing (elemIndex, getAt, splitAt, uniqueBy)
import Random exposing (Seed)
import RummikubEngine.Models exposing (..)
import RummikubEngine.Utils exposing (..)
import Tuple


newGameWithDefaults : Seed -> GameState
newGameWithDefaults seed =
    case newGame seed defaultGameConfig of
        Err msg ->
            -- TODO ideally impossible state
            { unflipped = [], playerHands = [], board = [], playerTurn = 0 }

        Ok state ->
            state


newGame : Seed -> GameConfig -> Result String GameState
newGame seed { numPlayers, tileDuplicates, startingPlayerTileCount } =
    case numPlayers < 1 of
        True ->
            Err "Must have a positive number of players"

        False ->
            case tileDuplicates < 1 of
                True ->
                    Err "Must have a positive number of tile duplicates"

                False ->
                    let
                        allTiles =
                            generateAllTiles tileDuplicates
                    in
                    case (startingPlayerTileCount * numPlayers) > List.length allTiles of
                        True ->
                            Err "Cannot allocate enough tiles per player"

                        False ->
                            let
                                ( unflipped, playerHands ) =
                                    List.foldl
                                        (\_ ( currUnflipped, currPlayerHands ) ->
                                            let
                                                ( newUnflipped, newPlayerHand ) =
                                                    takeTiles seed currUnflipped startingPlayerTileCount
                                            in
                                            ( newUnflipped, newPlayerHand :: currPlayerHands )
                                        )
                                        ( generateAllTiles tileDuplicates, [] )
                                        (List.range 0 (numPlayers - 1))
                            in
                            Ok
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
