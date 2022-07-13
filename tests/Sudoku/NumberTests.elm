module Sudoku.NumberTests exposing (..)

import Expect
import Sudoku.Number as Number
import Test exposing (..)


suite : Test
suite =
    describe "Sudoku.Number Module"
        [ describe "Sudoku.Number.fromChar"
            [ test "Returns 'Just Number' for valid digit characters" <|
                \_ ->
                    '3'
                        |> Number.fromChar
                        |> Expect.equal (Just Number.three)
            , test "Returns 'Nothing' for invalid characters" <|
                \_ ->
                    'a'
                        |> Number.fromChar
                        |> Expect.equal Nothing
            ]
        , describe "Sudoku.Number.toString"
            [ test "Returns the number as a string" <|
                \_ ->
                    Number.three
                        |> Number.toString
                        |> Expect.equal "3"
            ]
        ]
