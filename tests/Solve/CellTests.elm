module Solve.CellTests exposing (..)

import Expect
import Solve.Cell as Cell
import Sudoku.Number as Number
import Test exposing (..)


suite : Test
suite =
    describe "Solve.Cell Module"
        [ describe "Sudoku.Cell.initFromChar"
            [ test "Valid number input results in a given cell value" <|
                \_ ->
                    '3'
                        |> Cell.initFromChar
                        |> Cell.isGiven
                        |> Expect.equal True
            , describe "other inputs produce empty cells"
                [ test "Cell is empty" <|
                    \_ ->
                        '.'
                            |> Cell.initFromChar
                            |> Cell.isFilled
                            |> Expect.equal False
                , test "notes are blank" <|
                    \_ ->
                        '.'
                            |> Cell.initFromChar
                            |> Cell.getNotes
                            |> Expect.equal
                                (Just
                                    { primary = Number.emptySet
                                    , secondary = Number.emptySet
                                    }
                                )
                , test "Possibilities include all numbers" <|
                    \_ ->
                        '.'
                            |> Cell.initFromChar
                            |> Cell.getPossible
                            |> Expect.equal
                                (Just Number.fullSet)
                ]
            ]
        ]
