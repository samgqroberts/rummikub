module GameTests exposing (all, expectStartingPlayerTileCount, newGameInst, totalNumTiles)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import Game exposing (..)
import List.Extra exposing (getAt)
import Models exposing (..)
import Random
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
        , describe "allColorsTheSame"
            [ test "1 of the same color" <|
                \_ ->
                    allColorsTheSame [ ( Black, One ) ] |> Expect.equal True
            , test "2 of the same color" <|
                \_ ->
                    allColorsTheSame [ ( Black, One ), ( Black, Two ) ] |> Expect.equal True
            , test "3 of the same color" <|
                \_ ->
                    allColorsTheSame [ ( Black, One ), ( Black, Two ), ( Black, Three ) ] |> Expect.equal True
            , test "4 of the same color" <|
                \_ ->
                    allColorsTheSame [ ( Black, One ), ( Black, Two ), ( Black, Three ), ( Black, One ) ] |> Expect.equal True
            , test "3 of the same + 1 different" <|
                \_ ->
                    allColorsTheSame [ ( Black, One ), ( Black, Two ), ( Black, Three ), ( Blue, One ) ] |> Expect.equal False
            ]
        , describe "allColorsUnique"
            [ test "1 different colors" <|
                \_ ->
                    allColorsUnique [ ( Black, One ) ] |> Expect.equal True
            , test "2 different colors" <|
                \_ ->
                    allColorsUnique [ ( Black, One ), ( Blue, One ) ] |> Expect.equal True
            , test "3 different colors" <|
                \_ ->
                    allColorsUnique [ ( Black, One ), ( Blue, Two ), ( Red, Three ) ] |> Expect.equal True
            , test "4 different colors" <|
                \_ ->
                    allColorsUnique [ ( Black, One ), ( Blue, Two ), ( Red, Three ), ( Orange, One ) ] |> Expect.equal True
            , test "3 different + 1 duplicate" <|
                \_ ->
                    allColorsUnique [ ( Black, One ), ( Blue, Two ), ( Black, Three ), ( Red, One ) ] |> Expect.equal False
            , test "4 different + 1 duplicate" <|
                \_ ->
                    allColorsUnique [ ( Black, One ), ( Blue, Two ), ( Black, Three ), ( Red, One ), ( Orange, Five ) ] |> Expect.equal False
            ]
        , describe "isValidGroup"
            [ test "3 of the same number" <|
                \_ ->
                    isValidGroup [ ( Black, One ), ( Blue, One ), ( Orange, One ) ] |> Expect.equal True
            , test "4 of the same number" <|
                \_ ->
                    isValidGroup [ ( Blue, Two ), ( Orange, Two ), ( Black, Two ), ( Red, Two ) ] |> Expect.equal True
            , test "3 number run" <|
                \_ ->
                    isValidGroup [ ( Blue, Five ), ( Blue, Four ), ( Blue, Six ) ] |> Expect.equal True
            , test "13 number run" <|
                \_ ->
                    isValidGroup
                        [ ( Blue, One )
                        , ( Blue, Two )
                        , ( Blue, Three )
                        , ( Blue, Four )
                        , ( Blue, Five )
                        , ( Blue, Six )
                        , ( Blue, Seven )
                        , ( Blue, Eight )
                        , ( Blue, Nine )
                        , ( Blue, Ten )
                        , ( Blue, Eleven )
                        , ( Blue, Twelve )
                        , ( Blue, Thirteen )
                        ]
                        |> Expect.equal True
            , test "run with mismatched color" <|
                \_ ->
                    isValidGroup [ ( Orange, Ten ), ( Red, Eleven ), ( Orange, Twelve ), ( Orange, Thirteen ) ] |> Expect.equal False
            , test "run with gap" <|
                \_ ->
                    isValidGroup [ ( Red, Two ), ( Red, Four ), ( Red, Five ), ( Red, Six ) ] |> Expect.equal False
            , test "3 of same number + 1 duplicate" <|
                \_ ->
                    isValidGroup [ ( Red, Two ), ( Blue, Two ), ( Blue, Two ), ( Orange, Two ) ] |> Expect.equal False
            , test "4 of same number + 1 duplicate" <|
                \_ ->
                    isValidGroup [ ( Red, Two ), ( Blue, Two ), ( Blue, Two ), ( Orange, Two ), ( Black, Two ) ] |> Expect.equal False
            , test "0 tile group" <|
                \_ ->
                    isValidGroup [] |> Expect.equal False
            , test "1 tile group" <|
                \_ ->
                    isValidGroup [ ( Red, Two ) ] |> Expect.equal False
            , test "2 tile group" <|
                \_ ->
                    isValidGroup [ ( Red, Two ), ( Red, Three ) ] |> Expect.equal False
            ]
        , describe "isValidBoard"
            [ test "1 valid group" <|
                \_ ->
                    isValidBoard [ [ ( Red, Two ), ( Red, Three ), ( Red, Four ) ] ] |> Expect.equal True
            , test "2 valid groups" <|
                \_ ->
                    isValidBoard
                        [ [ ( Red, Two ), ( Red, Three ), ( Red, Four ) ]
                        , [ ( Blue, Thirteen ), ( Orange, Thirteen ), ( Black, Thirteen ) ]
                        ]
                        |> Expect.equal True
            , test "1 valid 1 invalid group" <|
                \_ ->
                    isValidBoard
                        [ [ ( Red, Two ), ( Red, Three ), ( Red, Four ) ]
                        , [ ( Blue, Thirteen ), ( Blue, Thirteen ), ( Black, Thirteen ) ]
                        ]
                        |> Expect.equal False
            ]
        , describe "listDiff"
            [ test "with one of any element and perfect containment" <|
                \_ ->
                    listDiff [ 1, 2, 3 ] [ 1, 2 ]
                        |> Expect.equal ( [ 3 ], [] )
            , test "with multiple equal elements" <|
                \_ ->
                    listDiff [ 1, 2, 2, 3, 3 ] [ 1, 2 ]
                        |> Expect.equal ( [ 2, 3, 3 ], [] )
            , test "with elements in subtrahend not present in minuend" <|
                \_ ->
                    listDiff [ 1, 2, 2 ] [ 2, 3 ]
                        |> Expect.equal ( [ 1, 2 ], [ 3 ] )
            , test "with empty minuend" <|
                \_ ->
                    listDiff [] [ 1 ]
                        |> Expect.equal ( [], [ 1 ] )
            , test "with empty subtrahend" <|
                \_ ->
                    listDiff [ 1, 2 ] []
                        |> Expect.equal ( [ 1, 2 ], [] )
            ]
        , describe "containsAll"
            [ test "with one of any element and perfect containment" <|
                \_ ->
                    containsAll [ 1, 2, 3 ] [ 1, 2 ]
                        |> Expect.equal True
            , test "with one of any element and imperfect containment" <|
                \_ ->
                    containsAll [ 1, 2, 3 ] [ 1, 2, 4 ]
                        |> Expect.equal False
            , test "with duplicate elements and perfect containment" <|
                \_ ->
                    containsAll [ 1, 2, 2, 3, 3 ] [ 1, 2, 2 ]
                        |> Expect.equal True
            , test "with dupliacte elements and imperfect containment" <|
                \_ ->
                    containsAll [ 1, 2, 2, 3, 3 ] [ 1, 2, 4 ]
                        |> Expect.equal False
            ]
        , describe "replaceAt"
            [ test "at first index" <|
                \_ ->
                    replaceAt 0 0 [ 1, 2, 3 ]
                        |> Expect.equal (Just [ 0, 2, 3 ])
            , test "at last index" <|
                \_ ->
                    replaceAt 2 0 [ 1, 2, 3 ]
                        |> Expect.equal (Just [ 1, 2, 0 ])
            , test "at intermediate index" <|
                \_ ->
                    replaceAt 1 0 [ 1, 2, 3 ]
                        |> Expect.equal (Just [ 1, 0, 3 ])
            , test "at out of bounds index (too low)" <|
                \_ ->
                    replaceAt -1 0 [ 1, 2, 3 ]
                        |> Expect.equal Nothing
            , test "at out of bounds index (too high)" <|
                \_ ->
                    replaceAt 3 0 [ 1, 2, 3 ]
                        |> Expect.equal Nothing
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