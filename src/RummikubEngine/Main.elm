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
            { unflipped = [], playerStates = [], board = [], playerTurn = 0 }

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
                                                    takeTiles currUnflipped startingPlayerTileCount
                                            in
                                            ( newUnflipped, newPlayerHand :: currPlayerHands )
                                        )
                                        (Tuple.mapFirst (shuffleList seed) ( generateAllTiles tileDuplicates, [] ))
                                        (List.range 0 (numPlayers - 1))
                            in
                            Ok
                                { unflipped = unflipped
                                , playerStates = List.map (\hand -> { hand = hand, hasPlayed = False }) playerHands
                                , board = []
                                , playerTurn = 0
                                }


getCurrentPlayerState : GameState -> PlayerState
getCurrentPlayerState gameState =
    -- TODO impossible state
    Maybe.withDefault { hand = [], hasPlayed = False } (getAt (getPlayerTurn gameState) (getPlayerStates gameState))


attemptMove : GameState -> Move -> Result String GameState
attemptMove current move =
    case move of
        TakeTile ->
            let
                currentPlayerState =
                    getCurrentPlayerState current

                currentPlayerHand =
                    getHand currentPlayerState

                ( newUnflipped, newCurrentPlayerHand ) =
                    moveTile ( current.unflipped, currentPlayerHand )

                -- TODO impossible state
                newPlayerStates =
                    Maybe.withDefault (getPlayerStates current) (replaceAt (getPlayerTurn current) { currentPlayerState | hand = newCurrentPlayerHand } (getPlayerStates current))
            in
            Ok
                { current
                    | playerStates = newPlayerStates
                    , unflipped = newUnflipped
                    , playerTurn = nextPlayerTurn current
                }

        Play newBoard ->
            let
                currentPlayerState =
                    getCurrentPlayerState current

                currentPlayerHand =
                    getHand currentPlayerState

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

                                newCurrentPlayerState =
                                    { currentPlayerState
                                        | hand = newCurrentPlayerHand
                                        , hasPlayed = True
                                    }

                                newPlayerStates =
                                    replaceAt (getPlayerTurn current) newCurrentPlayerState (getPlayerStates current)
                                        -- TODO impossible state
                                        |> Maybe.withDefault (getPlayerStates current)
                            in
                            Ok
                                { current
                                    | board = newBoard
                                    , playerStates = newPlayerStates
                                    , playerTurn = nextPlayerTurn current
                                }
