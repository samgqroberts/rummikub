module RummikubEngine.MainTests exposing (all, expectStartingPlayerTileCount, newGameInst, totalNumTiles)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import List.Extra exposing (getAt)
import Random
import RummikubEngine.Main exposing (..)
import RummikubEngine.Models exposing (..)
import RummikubEngine.Utils exposing (..)
import Test exposing (..)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


newGameInst seeder =
    newGame (Random.initialSeed seeder)


expectStartingPlayerTileCount : Int -> GameState -> Expectation
expectStartingPlayerTileCount playerIndex inst =
    Expect.equal (Maybe.map List.length (getAt playerIndex inst.playerHands)) (Just startingPlayerTileCount)


totalNumTiles =
    List.length allTiles


all : Test
all =
    describe "Game"
        [ test "starting number of tiles" <|
            \_ ->
                Expect.equal totalNumTiles (List.length colors * List.length numbers * tileDuplicates)
        , fuzz int "new game unflipped num" <|
            \seeder ->
                Expect.equal (List.length (newGameInst seeder).unflipped) (totalNumTiles - startingPlayerTileCount * defaultNumPlayers)
        , fuzz int "new game player num" <|
            \seeder ->
                Expect.equal (List.length (newGameInst seeder).playerHands) defaultNumPlayers
        , fuzz int "new game playerhand num tiles" <|
            \seeder ->
                Expect.all
                    [ expectStartingPlayerTileCount 0
                    , expectStartingPlayerTileCount 1
                    , expectStartingPlayerTileCount 2
                    , expectStartingPlayerTileCount 3
                    ]
                    (newGameInst seeder)
        , fuzz int "new game has 0 board tiles" <|
            \seeder ->
                Expect.equal (List.length (newGameInst seeder).board) 0
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
