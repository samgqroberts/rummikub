module RummikubSolver.SolverTests exposing (all)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import List.Extra exposing (getAt)
import Random
import RummikubEngine.Models exposing (..)
import RummikubSolver.Solver exposing (..)
import Test exposing (..)


emptyState : GameState
emptyState =
    { unflipped = [], board = [], playerStates = [], playerTurn = 0 }


all : Test
all =
    describe "RummikubSolver.Solver"
        [ test "does anything" <|
            \_ ->
                solve emptyState |> Expect.equal TakeTile
        ]
