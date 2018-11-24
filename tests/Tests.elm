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
    ]
