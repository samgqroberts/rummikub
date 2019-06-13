module RummikubSolver.Solver exposing (solve)

import RummikubEngine.Models exposing (..)


solve : GameState -> Move
solve gameState =
    TakeTile
