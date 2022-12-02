module Sudoku.GridTests exposing (..)

import Expect
import Sudoku.Grid as Grid
import Test exposing (..)


suite : Test
suite =
    describe "Sudoku.Grid Module"
        [ describe "indexToCoord"
            [ test "Valid index is correctly translated to a Coord" <|
                \_ ->
                    16
                        |> Grid.indexToCoord
                        |> Expect.equal { x = 7, y = 1 }
            ]
        , describe "coordToIndex"
            [ test "Valid Coord is correctly translated to an index" <|
                \_ ->
                    { x = 4, y = 6 }
                        |> Grid.coordToIndex
                        |> Expect.equal 58
            ]
        ]
