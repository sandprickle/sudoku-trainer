module Sudoku.Puzzle exposing
    ( Coord
    , Puzzle
    , fixCell
    , fromGrid
    , prune
    )

import Sudoku.Cell as Cell exposing (Cell(..))
import Sudoku.Grid as Grid exposing (Grid)
import Sudoku.Number as Number exposing (Number)


type Puzzle a
    = Puzzle Grid


type alias Coord =
    Grid.Coord


{-| A Puzzle that contains at least 17 given numbers
and follows the constraints of Sudoku
-}
type Valid
    = Valid


{-| A Valid Puzzle that has been pruned
-}
type Pruned
    = Pruned


type Problem
    = NotSolvable
    | IllegalPuzzle
    | IllegalNumber Coord


fixCell : Number -> Coord -> Puzzle Valid -> Result Problem (Puzzle Valid)
fixCell newNum coord (Puzzle grid) =
    let
        oldCell =
            Grid.getByCoord coord grid
    in
    case oldCell of
        Given _ ->
            Ok (Puzzle grid)

        Fixed _ _ ->
            Ok (Puzzle grid)

        Possible _ notes ->
            let
                newPuzzle =
                    Puzzle (Grid.setByCoord coord grid (Fixed newNum notes))
            in
            case check newPuzzle of
                Ok puzzle ->
                    Ok puzzle

                Err _ ->
                    Err (IllegalNumber coord)


clearCell : Coord -> Puzzle Valid -> Puzzle Valid
clearCell coord (Puzzle grid) =
    let
        oldCell =
            Grid.getByCoord coord grid
    in
    case oldCell of
        Given _ ->
            Puzzle grid

        Fixed _ notes ->
            Puzzle (Grid.setByCoord coord grid Cell.default)

        Possible _ notes ->
            Puzzle grid


check : Puzzle a -> Result Problem (Puzzle Valid)
check (Puzzle grid) =
    if Grid.isSolvable grid then
        if Grid.isLegal grid then
            Ok (Puzzle grid)

        else
            Err IllegalPuzzle

    else
        Err NotSolvable


prune : Puzzle Valid -> Puzzle Pruned
prune (Puzzle grid) =
    Puzzle (Grid.pruneAll grid)


fromGrid : Grid -> Result Problem (Puzzle Valid)
fromGrid grid =
    check (Puzzle grid)
