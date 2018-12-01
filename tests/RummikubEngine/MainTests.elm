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
    List.length (generateAllTiles defaultTileDuplicates)


type alias GameGenerator =
    Int -> Result String GameState


defaultGameConfigWithNumPlayers : Int -> GameConfig
defaultGameConfigWithNumPlayers numPlayers =
    { numPlayers = numPlayers, tileDuplicates = defaultTileDuplicates }


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


testAllPlayersHaveCorrectNumberOfTiles : GameGenerator -> Test
testAllPlayersHaveCorrectNumberOfTiles getNewGame =
    newGameTest "all players have the correct number of tiles" getNewGame <|
        \state ->
            state.playerHands
                |> List.all (\hand -> List.length hand == defaultStartingPlayerTileCount)
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


newGameTestSuite : GameConfig -> List Test
newGameTestSuite config =
    List.map
        (\test -> test (newGameGenerator config))
        [ testNumberOfPlayerHandsIsCorrect config
        , testAllPlayersHaveCorrectNumberOfTiles
        , testCorrectTotalNumberOfTilesInGame config
        , testNoPlayedTiles
        ]


all : Test
all =
    describe "RummikubEngine.Main"
        [ describe "newGame"
            [ describe "with 1 player" <|
                newGameTestSuite (defaultGameConfigWithNumPlayers 1)
            , describe "with 2 players" <|
                newGameTestSuite (defaultGameConfigWithNumPlayers 2)
            , describe "with 3 players" <|
                newGameTestSuite (defaultGameConfigWithNumPlayers 3)
            , describe "with 4 players" <|
                newGameTestSuite (defaultGameConfigWithNumPlayers 4)
            , describe "with 5 players" <|
                newGameTestSuite (defaultGameConfigWithNumPlayers 5)
            , describe "with 6 players" <|
                newGameTestSuite (defaultGameConfigWithNumPlayers 6)
            , describe "with tileDuplicates of 1" <|
                newGameTestSuite { numPlayers = 2, tileDuplicates = 1 }
            , describe "with tileDuplicates of 2" <|
                newGameTestSuite { numPlayers = 2, tileDuplicates = 2 }
            , describe "with tileDuplicates of 3" <|
                newGameTestSuite { numPlayers = 2, tileDuplicates = 3 }
            , test "with 0 players results in error" <|
                \_ ->
                    newGame defaultInitialSeed (defaultGameConfigWithNumPlayers 0)
                        |> Expect.equal (Err "Must have a positive number of players")
            , test "with -1 players results in error" <|
                \_ ->
                    newGame defaultInitialSeed (defaultGameConfigWithNumPlayers -1)
                        |> Expect.equal (Err "Must have a positive number of players")
            , test "with 0 tile duplicates results in error" <|
                \_ ->
                    newGame defaultInitialSeed { numPlayers = 2, tileDuplicates = 0 }
                        |> Expect.equal (Err "Must have a positive number of tile duplicates")
            , test "too many players vs. tiles results in error" <|
                \_ ->
                    newGame defaultInitialSeed (defaultGameConfigWithNumPlayers 20)
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
