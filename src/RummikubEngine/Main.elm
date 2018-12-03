module RummikubEngine.Main exposing (attemptMove, newGame, newGameWithDefaults, playerHasWon)

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


playerHasWon : GameState -> Maybe Int
playerHasWon gameState =
    gameState
        |> getPlayerStates
        |> List.indexedMap Tuple.pair
        |> List.Extra.find (\( _, playerState ) -> List.isEmpty (getHand playerState))
        |> Maybe.map Tuple.first


newGame : Seed -> GameConfig -> Result String GameState
newGame seed { numPlayers, tileDuplicates, startingPlayerTileCount, numJokers } =
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
                            generateAllTiles tileDuplicates numJokers
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
                                        (Tuple.mapFirst (shuffleList seed) ( allTiles, [] ))
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


getCurrentPlayerHand : GameState -> PlayerHand
getCurrentPlayerHand gameState =
    getCurrentPlayerState gameState
        |> getHand


getCurrentPlayerHasPlayed : GameState -> Bool
getCurrentPlayerHasPlayed gameState =
    getCurrentPlayerState gameState
        |> getHasPlayed


tilesNotInHandError : Result String GameState
tilesNotInHandError =
    Err "Some played tiles are not in the player's hand"


invalidGroupsError : Result String GameState
invalidGroupsError =
    Err "Some played groups are not valid"


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

        InitialPlay groups ->
            case getCurrentPlayerHasPlayed current of
                True ->
                    Err "Current player has already made an Initial Play"

                False ->
                    case getInitialPlayPointValue groups of
                        Nothing ->
                            invalidGroupsError

                        Just pointValue ->
                            case pointValue < 30 of
                                True ->
                                    Err "Initial Play must have point value of 30 or more"

                                False ->
                                    case containsAllTiles (getCurrentPlayerHand current) (flattenGroups groups) of
                                        False ->
                                            tilesNotInHandError

                                        True ->
                                            let
                                                currentPlayerTurn =
                                                    getPlayerTurn current

                                                currentPlayerState =
                                                    getCurrentPlayerState current

                                                newCurrentPlayerState =
                                                    { currentPlayerState
                                                        | hasPlayed = True
                                                        , hand = tileListDiff (getHand currentPlayerState) (flattenGroups groups) |> Tuple.first
                                                    }

                                                newPlayerStates =
                                                    Maybe.withDefault (getPlayerStates current) (replaceAt currentPlayerTurn newCurrentPlayerState (getPlayerStates current))

                                                newBoard =
                                                    List.concat [ groups, getBoard current ]
                                            in
                                            Ok
                                                { current
                                                    | playerTurn = nextPlayerTurn current
                                                    , playerStates = newPlayerStates
                                                    , board = newBoard
                                                }

        Play newBoard ->
            case getCurrentPlayerHasPlayed current of
                False ->
                    Err "Current player must make an Initial Play first"

                True ->
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
                            tileListDiff newBoardTiles currentBoardTiles
                    in
                    case containsAllTiles currentPlayerHand playedTiles of
                        False ->
                            tilesNotInHandError

                        True ->
                            case isValidBoard newBoard of
                                False ->
                                    invalidGroupsError

                                True ->
                                    let
                                        ( newCurrentPlayerHand, _ ) =
                                            tileListDiff currentPlayerHand playedTiles

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
