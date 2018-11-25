module Tests exposing (..)

import Random
import List.Extra exposing (getAt)
import Game exposing (..)
import Test exposing (..)
import Fuzz exposing (..)
import Models exposing (..)
import Expect exposing (Expectation)


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!

newGameInst seeder = newGame (Random.initialSeed seeder)

expectStartingPlayerTileCount : Int -> GameState -> Expectation
expectStartingPlayerTileCount playerIndex inst =
  Expect.equal (Maybe.map List.length (getAt playerIndex inst.playerHands)) (Just startingPlayerTileCount)

totalNumTiles = List.length allTiles

all : Test
all =
  describe "Rummikub"
    [ test "starting number of tiles" <|
        \_ ->
          Expect.equal (totalNumTiles) (List.length colors * List.length numbers * tileDuplicates)
    , fuzz int "new game unflipped num" <|
        \seeder ->
          Expect.equal (List.length (newGameInst seeder).unflipped) (totalNumTiles - startingPlayerTileCount * numPlayers)
    , fuzz int "new game player num" <|
        \seeder ->
          Expect.equal (List.length (newGameInst seeder).playerHands) (numPlayers)
    , fuzz int "new game playerhand num tiles" <|
        \seeder ->
          Expect.all
            [ expectStartingPlayerTileCount 0
            , expectStartingPlayerTileCount 1
            , expectStartingPlayerTileCount 2
            , expectStartingPlayerTileCount 3
            ] (newGameInst seeder)
    , fuzz int "new game has 0 played tiles" <|
        \seeder ->
          Expect.equal (List.length (newGameInst seeder).played) 0
    , describe "allColorsTheSame"
      [ test "1 of the same color" <|
          \_ ->
            allColorsTheSame [(Black, One)] |> Expect.equal True
      , test "2 of the same color" <|
          \_ ->
            allColorsTheSame [(Black, One), (Black, Two)] |> Expect.equal True
      , test "3 of the same color" <|
          \_ ->
            allColorsTheSame [(Black, One), (Black, Two), (Black, Three)] |> Expect.equal True
      , test "4 of the same color" <|
          \_ ->
            allColorsTheSame [(Black, One), (Black, Two), (Black, Three), (Black, One)] |> Expect.equal True
      , test "3 of the same + 1 different" <|
          \_ ->
            allColorsTheSame [(Black, One), (Black, Two), (Black, Three), (Blue, One)] |> Expect.equal False
      ]
    , describe "allColorsUnique"
      [ test "1 different colors" <|
          \_ ->
            allColorsUnique [(Black, One)] |> Expect.equal True
      , test "2 different colors" <|
          \_ ->
            allColorsUnique [(Black, One), (Blue, One)] |> Expect.equal True
      , test "3 different colors" <|
          \_ ->
            allColorsUnique [(Black, One), (Blue, Two), (Red, Three)] |> Expect.equal True
      , test "4 different colors" <|
          \_ ->
            allColorsUnique [(Black, One), (Blue, Two), (Red, Three), (Orange, One)] |> Expect.equal True
      , test "3 different + 1 duplicate" <|
          \_ ->
            allColorsUnique [(Black, One), (Blue, Two), (Black, Three), (Red, One)] |> Expect.equal False
      , test "4 different + 1 duplicate" <|
          \_ ->
            allColorsUnique [(Black, One), (Blue, Two), (Black, Three), (Red, One), (Orange, Five)] |> Expect.equal False
      ]
    , describe "isValidGroup"
      [ test "3 of the same number" <|
          \_ ->
            isValidGroup [(Black, One), (Blue, One), (Orange, One)] |> Expect.equal True
      , test "4 of the same number" <|
          \_ ->
            isValidGroup [(Blue, Two), (Orange, Two), (Black, Two), (Red, Two)] |> Expect.equal True
      , test "3 number run" <|
          \_ ->
            isValidGroup [(Blue, Five), (Blue, Four), (Blue, Six)] |> Expect.equal True
      , test "13 number run" <|
          \_ ->
            isValidGroup [
              (Blue, One), (Blue, Two), (Blue, Three), (Blue, Four), (Blue, Five),
              (Blue, Six), (Blue, Seven), (Blue, Eight), (Blue, Nine), (Blue, Ten),
              (Blue, Eleven), (Blue, Twelve), (Blue, Thirteen)
            ] |> Expect.equal True
      , test "run with mismatched color" <|
          \_ ->
            isValidGroup [(Orange, Ten), (Red, Eleven), (Orange, Twelve), (Orange, Thirteen)] |> Expect.equal False
      , test "run with gap" <|
          \_ ->
            isValidGroup [(Red, Two), (Red, Four), (Red, Five), (Red, Six)] |> Expect.equal False
      , test "3 of same number + 1 duplicate" <|
          \_ ->
            isValidGroup [(Red, Two), (Blue, Two), (Blue, Two), (Orange, Two)] |> Expect.equal False
      , test "4 of same number + 1 duplicate" <|
          \_ ->
            isValidGroup [(Red, Two), (Blue, Two), (Blue, Two), (Orange, Two), (Black, Two)] |> Expect.equal False
      , test "0 tile group" <|
          \_ ->
            isValidGroup [] |> Expect.equal False
      , test "1 tile group" <|
          \_ ->
            isValidGroup [(Red, Two)] |> Expect.equal False
      , test "2 tile group" <|
          \_ ->
            isValidGroup [(Red, Two), (Red, Three)] |> Expect.equal False
      ]
    ]
