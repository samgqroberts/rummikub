module MainTests exposing (..)

import Random
import List.Extra exposing (getAt)
import Main exposing (initialState)
import Test exposing (..)
import Fuzz exposing (..)
import Models exposing (..)
import Expect exposing (Expectation)

all : Test
all =
  describe "Main"
    [ test "initialState imports" <|
        \_ ->
          let
            initialSt = initialState {- TODO this test basically just imports -}
          in
            Expect.pass
    ]
