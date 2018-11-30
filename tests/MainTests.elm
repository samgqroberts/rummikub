module MainTests exposing (all)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import List.Extra exposing (getAt)
import Main exposing (initialState)
import Models exposing (..)
import Random
import Test exposing (..)


all : Test
all =
    describe "Main"
        [ test "initialState imports" <|
            \_ ->
                let
                    initialSt =
                        initialState

                    {- TODO this test basically just imports -}
                in
                Expect.pass
        ]
