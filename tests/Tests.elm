module Tests exposing (..)

import Random
import List.Extra exposing (getAt)
import Game exposing (..)
import Test exposing (..)
import Models exposing (..)
import Expect exposing (Expectation)


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!

newGameInst = newGame (Random.initialSeed 1)

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
    , test "new game unflipped num" <|
        \_ ->
          Expect.equal (List.length newGameInst.unflipped) (totalNumTiles - startingPlayerTileCount * numPlayers)
    , test "new game player num" <|
        \_ ->
          Expect.equal (List.length newGameInst.playerHands) (numPlayers)
    , test "new game playerhand num tiles" <|
        \_ ->
          Expect.all
            [ expectStartingPlayerTileCount 0
            , expectStartingPlayerTileCount 1
            , expectStartingPlayerTileCount 2
            , expectStartingPlayerTileCount 3
            ] newGameInst
    , test "new game has 0 played tiles" <|
      \_ ->
        Expect.equal (List.length newGameInst.played) 0
    ]
