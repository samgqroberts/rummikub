module RummikubEngine.UtilsTests exposing (all)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import List.Extra exposing (getAt)
import Random
import RummikubEngine.Models exposing (..)
import RummikubEngine.Utils exposing (..)
import Test exposing (..)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Game.Utils"
        [ describe "allUniqueTiles"
            [ test "has correct size" <|
                \_ ->
                    List.length allUniqueTiles
                        |> Expect.equal
                            (List.length colors
                                * List.length numbers
                            )
            ]
        , describe "generateAllTiles"
            [ test "has correct size with no duplication" <|
                \_ ->
                    generateAllTiles 1 0
                        |> List.length
                        |> Expect.equal (List.length allUniqueTiles)
            , test "has correct size with 2 duplicates per" <|
                \_ ->
                    generateAllTiles 2 0
                        |> List.length
                        |> Expect.equal
                            (List.length allUniqueTiles
                                * 2
                            )
            , test "has correct size with 3 duplicates per" <|
                \_ ->
                    generateAllTiles 3 0
                        |> List.length
                        |> Expect.equal
                            (List.length allUniqueTiles
                                * 3
                            )
            ]
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
                    isValidGroup (cards [ ( Black, One ), ( Blue, One ), ( Orange, One ) ]) |> Expect.equal True
            , test "4 of the same number" <|
                \_ ->
                    isValidGroup (cards [ ( Blue, Two ), ( Orange, Two ), ( Black, Two ), ( Red, Two ) ]) |> Expect.equal True
            , test "3 number run" <|
                \_ ->
                    isValidGroup (cards [ ( Blue, Five ), ( Blue, Four ), ( Blue, Six ) ]) |> Expect.equal True
            , test "13 number run" <|
                \_ ->
                    isValidGroup
                        (cards
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
                        )
                        |> Expect.equal True
            , test "run with mismatched color" <|
                \_ ->
                    isValidGroup (cards [ ( Orange, Ten ), ( Red, Eleven ), ( Orange, Twelve ), ( Orange, Thirteen ) ]) |> Expect.equal False
            , test "run with gap" <|
                \_ ->
                    isValidGroup (cards [ ( Red, Two ), ( Red, Four ), ( Red, Five ), ( Red, Six ) ]) |> Expect.equal False
            , test "3 of same number + 1 duplicate" <|
                \_ ->
                    isValidGroup (cards [ ( Red, Two ), ( Blue, Two ), ( Blue, Two ), ( Orange, Two ) ]) |> Expect.equal False
            , test "4 of same number + 1 duplicate" <|
                \_ ->
                    isValidGroup (cards [ ( Red, Two ), ( Blue, Two ), ( Blue, Two ), ( Orange, Two ), ( Black, Two ) ]) |> Expect.equal False
            , test "0 tile group" <|
                \_ ->
                    isValidGroup [] |> Expect.equal False
            , test "1 tile group" <|
                \_ ->
                    isValidGroup [ Card ( Red, Two ) ] |> Expect.equal False
            , test "2 tile group" <|
                \_ ->
                    isValidGroup (cards [ ( Red, Two ), ( Red, Three ) ]) |> Expect.equal False
            ]
        , describe "isValidBoard"
            [ test "1 valid group" <|
                \_ ->
                    isValidBoard [ cards [ ( Red, Two ), ( Red, Three ), ( Red, Four ) ] ] |> Expect.equal True
            , test "2 valid groups" <|
                \_ ->
                    isValidBoard
                        [ cards [ ( Red, Two ), ( Red, Three ), ( Red, Four ) ]
                        , cards [ ( Blue, Thirteen ), ( Orange, Thirteen ), ( Black, Thirteen ) ]
                        ]
                        |> Expect.equal True
            , test "1 valid 1 invalid group" <|
                \_ ->
                    isValidBoard
                        [ cards [ ( Red, Two ), ( Red, Three ), ( Red, Four ) ]
                        , cards [ ( Blue, Thirteen ), ( Blue, Thirteen ), ( Black, Thirteen ) ]
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
        ]
