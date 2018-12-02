module RummikubEngine.MainTests exposing (all)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import List.Extra exposing (getAt)
import Random exposing (Seed)
import RummikubEngine.Main exposing (..)
import RummikubEngine.Models exposing (..)
import RummikubEngine.Utils exposing (..)
import Test exposing (..)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


defaultInitialSeed =
    Random.initialSeed 10


defaultTotalNumTiles =
    List.length (generateAllTiles defaultGameConfig.tileDuplicates)


type alias GameGenerator =
    Int -> Result String GameState


newGameGenerator : GameConfig -> Int -> Result String GameState
newGameGenerator config seeder =
    newGame (Random.initialSeed seeder) config


newGameTest : String -> GameGenerator -> (GameState -> Expectation) -> Test
newGameTest testMsg getNewGame expectFromGameState =
    fuzz int testMsg <|
        \seeder ->
            case getNewGame seeder of
                Err msg ->
                    Expect.fail msg

                Ok gameState ->
                    expectFromGameState gameState


testNumberOfPlayerHandsIsCorrect : GameConfig -> GameGenerator -> Test
testNumberOfPlayerHandsIsCorrect { numPlayers } getNewGame =
    newGameTest "number of player hands is correct" getNewGame <|
        \state ->
            state
                |> getNumPlayers
                |> Expect.equal numPlayers


testAllPlayersHaveCorrectNumberOfTiles : GameConfig -> GameGenerator -> Test
testAllPlayersHaveCorrectNumberOfTiles { startingPlayerTileCount } getNewGame =
    newGameTest "all players have the correct number of tiles" getNewGame <|
        \state ->
            getPlayerHands state
                |> List.all (\hand -> List.length hand == startingPlayerTileCount)
                |> Expect.equal True


testCorrectTotalNumberOfTilesInGame : GameConfig -> GameGenerator -> Test
testCorrectTotalNumberOfTilesInGame { tileDuplicates } getNewGame =
    newGameTest "total number of tiles in the game is correct" getNewGame <|
        \state ->
            state
                |> getAllTilesCount
                |> Expect.equal (List.length (generateAllTiles tileDuplicates))


testNoPlayedTiles : GameGenerator -> Test
testNoPlayedTiles getNewGame =
    newGameTest "there are no played tiles" getNewGame <|
        \state ->
            state.board
                |> List.length
                |> Expect.equal 0


testNoPlayerHasPlayed : GameGenerator -> Test
testNoPlayerHasPlayed getNewGame =
    newGameTest "no player has played" getNewGame <|
        \state ->
            state
                |> getPlayerStates
                |> List.map .hasPlayed
                |> List.all (\a -> a == False)
                |> Expect.equal True


getAllTiles : GameState -> List Tile
getAllTiles gameState =
    [ gameState.board
    , getPlayerHands gameState
    ]
        |> List.concat
        |> List.concatMap (\a -> a)
        |> List.append gameState.unflipped


getAllTilesCount : GameState -> Int
getAllTilesCount gameState =
    getAllTiles gameState
        |> List.length


newGameTestSuite : GameConfig -> List Test
newGameTestSuite config =
    List.map
        (\test -> test (newGameGenerator config))
        [ testNumberOfPlayerHandsIsCorrect config
        , testAllPlayersHaveCorrectNumberOfTiles config
        , testCorrectTotalNumberOfTilesInGame config
        , testNoPlayedTiles
        , testNoPlayerHasPlayed
        ]


neverPlayed : PlayerHand -> PlayerState
neverPlayed playerHand =
    { hand = playerHand, hasPlayed = False }


hasPlayed : PlayerHand -> PlayerState
hasPlayed playerHand =
    { hand = playerHand, hasPlayed = True }


all : Test
all =
    describe "RummikubEngine.Main"
        [ describe "newGame"
            [ describe "with 1 player" <|
                newGameTestSuite { defaultGameConfig | numPlayers = 1 }
            , describe "with 2 players" <|
                newGameTestSuite { defaultGameConfig | numPlayers = 2 }
            , describe "with 3 players" <|
                newGameTestSuite { defaultGameConfig | numPlayers = 3 }
            , describe "with 4 players" <|
                newGameTestSuite { defaultGameConfig | numPlayers = 4 }
            , describe "with 5 players" <|
                newGameTestSuite { defaultGameConfig | numPlayers = 5 }
            , describe "with 6 players" <|
                newGameTestSuite { defaultGameConfig | numPlayers = 6 }
            , describe "with tileDuplicates of 1" <|
                newGameTestSuite { defaultGameConfig | numPlayers = 2, tileDuplicates = 1 }
            , describe "with tileDuplicates of 2" <|
                newGameTestSuite { defaultGameConfig | tileDuplicates = 2 }
            , describe "with tileDuplicates of 3" <|
                newGameTestSuite { defaultGameConfig | tileDuplicates = 3 }
            , describe "with startingPlayerTileCount of 10" <|
                newGameTestSuite { defaultGameConfig | startingPlayerTileCount = 10 }
            , describe "with startingPlayerTileCount of 14" <|
                newGameTestSuite { defaultGameConfig | startingPlayerTileCount = 14 }
            , describe "with startingPlayerTileCount of 18" <|
                newGameTestSuite { defaultGameConfig | startingPlayerTileCount = 18 }
            , describe "initial tile allocation validation" <|
                [ test "error due to high player count" <|
                    \_ ->
                        newGame defaultInitialSeed { defaultGameConfig | numPlayers = 20 }
                            |> Expect.equal (Err "Cannot allocate enough tiles per player")
                , test "error due to high starting player tile count" <|
                    \_ ->
                        newGame defaultInitialSeed { defaultGameConfig | startingPlayerTileCount = 100 }
                            |> Expect.equal (Err "Cannot allocate enough tiles per player")
                , test "error due to low tileDuplicates" <|
                    \_ ->
                        newGame defaultInitialSeed { defaultGameConfig | tileDuplicates = 1 }
                            |> Expect.equal (Err "Cannot allocate enough tiles per player")
                ]
            , test "with 0 players results in error" <|
                \_ ->
                    newGame defaultInitialSeed { defaultGameConfig | numPlayers = 0 }
                        |> Expect.equal (Err "Must have a positive number of players")
            , test "with -1 players results in error" <|
                \_ ->
                    newGame defaultInitialSeed { defaultGameConfig | numPlayers = -1 }
                        |> Expect.equal (Err "Must have a positive number of players")
            , test "with 0 tile duplicates results in error" <|
                \_ ->
                    newGame defaultInitialSeed { defaultGameConfig | numPlayers = 2, tileDuplicates = 0 }
                        |> Expect.equal (Err "Must have a positive number of tile duplicates")
            ]
        , describe "attemptMove"
            [ describe "TakeTile"
                [ test "with enough tiles left" <|
                    \_ ->
                        let
                            current =
                                { unflipped = [ ( Red, Four ), ( Blue, Nine ) ]
                                , board = []
                                , playerStates = [ neverPlayed [ ( Red, Two ), ( Red, Three ) ], neverPlayed [] ]
                                , playerTurn = 0
                                }
                        in
                        attemptMove current TakeTile
                            |> Expect.equal
                                (Ok
                                    { unflipped = [ ( Blue, Nine ) ]
                                    , board = []
                                    , playerStates = [ neverPlayed [ ( Red, Four ), ( Red, Two ), ( Red, Three ) ], neverPlayed [] ]
                                    , playerTurn = 1
                                    }
                                )
                , test "without enough tiles left (pass)" <|
                    \_ ->
                        let
                            current =
                                { unflipped = []
                                , board = []
                                , playerStates = [ neverPlayed [ ( Red, Two ), ( Red, Three ) ], neverPlayed [] ]
                                , playerTurn = 0
                                }
                        in
                        attemptMove current TakeTile
                            |> Expect.equal
                                (Ok
                                    { unflipped = []
                                    , board = []
                                    , playerStates = [ neverPlayed [ ( Red, Two ), ( Red, Three ) ], neverPlayed [] ]
                                    , playerTurn = 1
                                    }
                                )
                ]
            , test "using tiles not in the player's hand" <|
                \_ ->
                    let
                        current =
                            { unflipped = []
                            , board = []
                            , playerStates = [ neverPlayed [ ( Red, Two ), ( Red, Three ) ] ]
                            , playerTurn = 0
                            }

                        newBoard =
                            [ [ ( Red, Two ), ( Red, Three ), ( Red, Four ) ] ]
                    in
                    attemptMove current (Play newBoard)
                        |> Expect.equal (Err "Some played tiles are not in the player's hand")
            , test "playing some invalid groups" <|
                \_ ->
                    let
                        current =
                            { unflipped = []
                            , board = []
                            , playerStates = [ neverPlayed [ ( Red, Two ), ( Red, Three ) ] ]
                            , playerTurn = 0
                            }

                        newBoard =
                            [ [ ( Red, Two ), ( Red, Three ) ] ]
                    in
                    attemptMove current (Play newBoard)
                        |> Expect.equal (Err "Some played groups are not valid")
            , test "valid move" <|
                \_ ->
                    let
                        current =
                            { unflipped = []
                            , board = []
                            , playerStates =
                                [ neverPlayed [ ( Red, Two ), ( Red, Three ), ( Red, Four ) ]
                                , neverPlayed []
                                ]
                            , playerTurn = 0
                            }

                        newBoard =
                            [ [ ( Red, Two ), ( Red, Three ), ( Red, Four ) ] ]
                    in
                    attemptMove current (Play newBoard)
                        |> Expect.equal
                            (Ok
                                { unflipped = []
                                , board = newBoard
                                , playerStates = [ hasPlayed [], neverPlayed [] ]
                                , playerTurn = 1
                                }
                            )
            ]
        ]
