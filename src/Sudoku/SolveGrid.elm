port module Sudoku.SolveGrid exposing
    ( Coord
    , SolveGrid
    , coordDecoder
    , decoder
    , empty
    , encode
    , encodeCoord
    , fromJson
    , getByCoord
    , isLegal
    , isSolvable
    , legal
    , load
    , pruneAll
    , receiver
    , resetPossible
    , save
    , setByCoord
    , solvable
    , toBoxes
    , toCols
    , toRows
    )

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra
import Sudoku.Cell as Cell exposing (Cell(..))
import Sudoku.Grid as Grid exposing (Grid)
import Sudoku.Number as Number exposing (NumSet)


type alias SolveGrid =
    Grid Cell



-- Validity Checks


solvable : SolveGrid -> Maybe SolveGrid
solvable grid =
    if isSolvable grid then
        Just grid

    else
        Nothing


isSolvable : SolveGrid -> Bool
isSolvable grid =
    Grid.isSolvable Cell.getNumber grid


legal : SolveGrid -> Maybe SolveGrid
legal grid =
    if isLegal grid then
        Just grid

    else
        Nothing


isLegal : SolveGrid -> Bool
isLegal grid =
    Grid.isLegal Cell.getNumber grid



-- Get/set by Coord


getByCoord : Coord -> SolveGrid -> Cell
getByCoord =
    Grid.getByCoord


setByCoord : Coord -> SolveGrid -> Cell -> SolveGrid
setByCoord =
    Grid.setByCoord



-- Convert to List of Rows, Cols, or Boxes


toRows : SolveGrid -> List (List Cell)
toRows =
    Grid.toRows


toCols : SolveGrid -> List (List Cell)
toCols =
    Grid.toCols


toBoxes : SolveGrid -> List (List Cell)
toBoxes =
    Grid.toBoxes



-- Row, col, or box Coords as a List


rowCoords : Int -> List Coord
rowCoords =
    Grid.rowCoords


colCoords : Int -> List Coord
colCoords =
    Grid.colCoords


boxCoords : Int -> List Coord
boxCoords =
    Grid.boxCoords



-- Possible Value Pruning


pruneAll : SolveGrid -> SolveGrid
pruneAll grid =
    let
        newGrid =
            grid
                |> pruneRows
                |> pruneCols
                |> pruneBoxes
    in
    if grid == newGrid then
        grid

    else
        pruneAll newGrid


pruneRows : SolveGrid -> SolveGrid
pruneRows grid =
    let
        rows =
            List.map rowCoords (List.range 0 8)
    in
    List.foldl pruneReducer grid rows


pruneCols : SolveGrid -> SolveGrid
pruneCols grid =
    let
        cols =
            List.map colCoords (List.range 0 8)
    in
    List.foldl pruneReducer grid cols


pruneBoxes : SolveGrid -> SolveGrid
pruneBoxes grid =
    let
        boxes =
            List.map boxCoords (List.range 0 8)
    in
    List.foldl pruneReducer grid boxes


pruneReducer : List Coord -> SolveGrid -> SolveGrid
pruneReducer coords grid =
    let
        fn : ( Coord, Cell ) -> SolveGrid -> SolveGrid
        fn ( coord, cell ) grid_ =
            setByCoord coord grid_ cell
    in
    coords
        |> List.map (\coord -> getByCoord coord grid)
        |> pruneCells
        |> List.map2 (\coord cell -> ( coord, cell )) coords
        |> List.foldl fn grid


pruneCells : List Cell -> List Cell
pruneCells cells =
    let
        fixedNumbers =
            List.filterMap Cell.getNumber cells |> Number.setFromList

        updatePossible : Cell -> NumSet -> Cell
        updatePossible cell_ numbers =
            Cell.setPossible numbers cell_

        pruneCell : Cell -> Cell
        pruneCell cell =
            case Cell.getPossible cell of
                Just possible ->
                    Number.setDiff possible fixedNumbers
                        |> updatePossible cell

                Nothing ->
                    cell
    in
    List.map pruneCell cells


empty : SolveGrid
empty =
    Grid.init Cell.default



-- Puzzle Logic


resetPossible : SolveGrid -> SolveGrid
resetPossible grid =
    grid
        |> Grid.map
            (\cell ->
                case cell of
                    Fixed _ _ ->
                        cell

                    Given _ ->
                        cell

                    Possible _ notes ->
                        Possible Number.setAll notes
            )



-- Encode/Decode


encode : SolveGrid -> Encode.Value
encode =
    Grid.encode Cell.encode


decoder : Decoder SolveGrid
decoder =
    Grid.decoder Cell.decoder


fromJson : Encode.Value -> Maybe SolveGrid
fromJson json =
    case Decode.decodeValue decoder json of
        Ok grid ->
            Just grid

        Err _ ->
            Nothing



-- PERSISTANCE


port saveGrid : Decode.Value -> Cmd msg


port loadGrid : () -> Cmd msg


port gridReceiver : (Decode.Value -> msg) -> Sub msg


save : SolveGrid -> Cmd msg
save grid =
    saveGrid (encode grid)


load : () -> Cmd msg
load _ =
    loadGrid ()


receiver : (Maybe SolveGrid -> msg) -> Sub msg
receiver fromGrid =
    gridReceiver (\json -> fromJson json |> fromGrid)



-- Coord Type


type alias Coord =
    Grid.Coord


encodeCoord : Coord -> Encode.Value
encodeCoord =
    Grid.encodeCoord


coordDecoder : Decoder Coord
coordDecoder =
    Grid.coordDecoder
