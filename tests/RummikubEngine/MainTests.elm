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


newGameInst seeder =
    newGame (Random.initialSeed seeder) 4


expectStartingPlayerTileCount : Int -> GameState -> Expectation
expectStartingPlayerTileCount playerIndex inst =
    Expect.equal (Maybe.map List.length (getAt playerIndex inst.playerHands)) (Just defaultStartingPlayerTileCount)


defaultTotalNumTiles =
    List.length (generateAllTiles defaultTileDuplicates)


type alias GameGenerator =
    Int -> Result String GameState


newGameWithNumPlayers : Int -> Int -> Result String GameState
newGameWithNumPlayers numPlayers seeder =
    newGame (Random.initialSeed seeder) numPlayers


newGameTest : String -> GameGenerator -> (GameState -> Expectation) -> Test
newGameTest testMsg getNewGame expectFromGameState =
    fuzz int testMsg <|
        \seeder ->
            case getNewGame seeder of
                Err msg ->
                    Expect.fail msg

                Ok gameState ->
                    expectFromGameState gameState


testNumberOfPlayerHandsIsCorrect : Int -> GameGenerator -> Test
testNumberOfPlayerHandsIsCorrect expectedNumPlayerHands getNewGame =
    newGameTest "number of player hands is correct" getNewGame <|
        \state ->
            state
                |> numPlayers
                |> Expect.equal expectedNumPlayerHands


testAllPlayersHaveCorrectNumberOfTiles : GameGenerator -> Test
testAllPlayersHaveCorrectNumberOfTiles getNewGame =
    newGameTest "all players have the correct number of tiles" getNewGame <|
        \state ->
            state.playerHands
                |> List.all (\hand -> List.length hand == defaultStartingPlayerTileCount)
                |> Expect.equal True


testCorrectTotalNumberOfTilesInGame : GameGenerator -> Test
testCorrectTotalNumberOfTilesInGame getNewGame =
    newGameTest "total number of tiles in the game is correct" getNewGame <|
        \state ->
            state
                |> getAllTilesCount
                |> Expect.equal defaultTotalNumTiles


testNoPlayedTiles : GameGenerator -> Test
testNoPlayedTiles getNewGame =
    newGameTest "there are no played tiles" getNewGame <|
        \state ->
            state.board
                |> List.length
                |> Expect.equal 0


getAllTiles : GameState -> List Tile
getAllTiles gameState =
    [ gameState.board
    , gameState.playerHands
    ]
        |> List.concat
        |> List.concatMap (\a -> a)
        |> List.append gameState.unflipped


getAllTilesCount : GameState -> Int
getAllTilesCount gameState =
    getAllTiles gameState
        |> List.length


newGameTestSuite : Int -> List Test
newGameTestSuite numPlayers =
    List.map
        (\test -> test (newGameWithNumPlayers numPlayers))
        [ testNumberOfPlayerHandsIsCorrect numPlayers
        , testAllPlayersHaveCorrectNumberOfTiles
        , testCorrectTotalNumberOfTilesInGame
        , testNoPlayedTiles
        ]


all : Test
all =
    describe "RummikubEngine.Main"
        [ describe "newGame"
            [ describe "with 1 player" <|
                newGameTestSuite 1
            , describe "with 2 players" <|
                newGameTestSuite 2
            , describe "with 3 players" <|
                newGameTestSuite 3
            , describe "with 4 players" <|
                newGameTestSuite 4
            , describe "with 5 players" <|
                newGameTestSuite 5
            , describe "with 6 players" <|
                newGameTestSuite 6
            , test "with 0 players results in error" <|
                \_ ->
                    newGame defaultInitialSeed 0
                        |> Expect.equal (Err "Must have a positive number of players")
            , test "with -1 players results in error" <|
                \_ ->
                    newGame defaultInitialSeed -1
                        |> Expect.equal (Err "Must have a positive number of players")
            , test "too many players results in error" <|
                \_ ->
                    newGame defaultInitialSeed 20
                        |> Expect.equal (Err "Cannot allocate enough tiles per player")
            ]
        , describe "attemptMove"
            [ test "using tiles not in the player's hand" <|
                \_ ->
                    let
                        current =
                            { unflipped = []
                            , board = []
                            , playerHands = [ [ ( Red, Two ), ( Red, Three ) ] ]
                            , playerTurn = 0
                            }

                        newBoard =
                            [ [ ( Red, Two ), ( Red, Three ), ( Red, Four ) ] ]
                    in
                    attemptMove current newBoard
                        |> Expect.equal (Err "Some played tiles are not in the player's hand")
            , test "playing some invalid groups" <|
                \_ ->
                    let
                        current =
                            { unflipped = []
                            , board = []
                            , playerHands = [ [ ( Red, Two ), ( Red, Three ) ] ]
                            , playerTurn = 0
                            }

                        newBoard =
                            [ [ ( Red, Two ), ( Red, Three ) ] ]
                    in
                    attemptMove current newBoard
                        |> Expect.equal (Err "Some played groups are not valid")
            , test "valid move" <|
                \_ ->
                    let
                        current =
                            { unflipped = []
                            , board = []
                            , playerHands =
                                [ [ ( Red, Two ), ( Red, Three ), ( Red, Four ) ]
                                , []
                                ]
                            , playerTurn = 0
                            }

                        newBoard =
                            [ [ ( Red, Two ), ( Red, Three ), ( Red, Four ) ] ]
                    in
                    attemptMove current newBoard
                        |> Expect.equal
                            (Ok
                                { unflipped = []
                                , board = newBoard
                                , playerHands = [ [], [] ]
                                , playerTurn = 1
                                }
                            )
            ]
        ]
