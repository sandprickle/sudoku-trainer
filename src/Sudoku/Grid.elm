module Sudoku.Grid exposing
    ( Coord
    , Grid
    , coordDecoder
    , decoder
    , encode
    , encodeCoord
    , fromString
    , getByCoord
    , setByCoord
    , toBoxes
    , toCols
    , toRows
    )

import Array exposing (Array)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra
import Sudoku.Cell as Cell exposing (Cell)
import Sudoku.Number as Number exposing (Number)


type Grid
    = Grid (Array Cell)



-- New Grid from String


fromString : String -> Grid
fromString str =
    String.trim str
        |> String.left 81
        |> String.padRight 81 '.'
        |> String.toList
        |> List.map Cell.initFromChar
        |> Array.fromList
        |> Grid
        |> pruneAll



-- Get/set by Coord


getByCoord : Coord -> Grid -> Cell
getByCoord coord (Grid grid) =
    Array.get (coordToIndex coord) grid |> Maybe.withDefault Cell.default


setByCoord : Coord -> Grid -> Cell -> Grid
setByCoord coord (Grid grid) newCell =
    Array.set (coordToIndex coord) newCell grid |> Grid



-- Convert to List of Rows, Cols, or Boxes


toRows : Grid -> List (List Cell)
toRows grid =
    List.map (\n -> getRow n grid) (List.range 0 8)


toCols : Grid -> List (List Cell)
toCols grid =
    List.map (\n -> getCol n grid) (List.range 0 8)


toBoxes : Grid -> List (List Cell)
toBoxes grid =
    List.map (\n -> getBox n grid) (List.range 0 8)



-- Get a Specific row, col, or box as a List


getRow : Int -> Grid -> List Cell
getRow rowNum grid =
    List.map (\coord -> getByCoord coord grid) (rowCoords rowNum)


getCol : Int -> Grid -> List Cell
getCol colNum grid =
    List.map (\coord -> getByCoord coord grid) (colCoords colNum)


getBox : Int -> Grid -> List Cell
getBox boxNum grid =
    List.map (\coord -> getByCoord coord grid) (boxCoords boxNum)



-- Row, col, or box Coords as a List


rowCoords : Int -> List Coord
rowCoords rowNum =
    let
        getCoord i =
            { x = i, y = rowNum }
    in
    List.map getCoord (List.range 0 8)


colCoords : Int -> List Coord
colCoords colNum =
    let
        getCoord i =
            { x = colNum, y = i }
    in
    List.map getCoord (List.range 0 8)


boxCoords : Int -> List Coord
boxCoords boxNum =
    let
        boxRow =
            boxNum // 3

        boxCol =
            modBy 3 boxNum

        yCoords =
            List.map (\n -> (3 * boxRow) + n) (List.range 0 2)

        xCoords =
            List.map (\n -> (3 * boxCol) + n) (List.range 0 2)

        getCoord y x =
            { x = x, y = y }

        coordsInRow yCoord =
            List.map (getCoord yCoord) xCoords
    in
    List.map coordsInRow yCoords |> List.concat



-- Possible Value Pruning


pruneAll : Grid -> Grid
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


pruneRows : Grid -> Grid
pruneRows grid =
    let
        rows =
            List.map rowCoords (List.range 0 8)
    in
    List.foldl pruneReducer grid rows


pruneCols : Grid -> Grid
pruneCols grid =
    let
        cols =
            List.map colCoords (List.range 0 8)
    in
    List.foldl pruneReducer grid cols


pruneBoxes : Grid -> Grid
pruneBoxes grid =
    let
        boxes =
            List.map boxCoords (List.range 0 8)
    in
    List.foldl pruneReducer grid boxes


pruneReducer : List Coord -> Grid -> Grid
pruneReducer coords grid =
    let
        fn : ( Coord, Cell ) -> Grid -> Grid
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
            List.filterMap Cell.getNumber cells

        updatePossible : Cell -> List Number -> Cell
        updatePossible cell_ numbers =
            Cell.setPossible numbers cell_

        pruneCell : Cell -> Cell
        pruneCell cell =
            case Cell.getPossible cell of
                Just possible ->
                    possible
                        |> removeFixed fixedNumbers
                        |> updatePossible cell

                Nothing ->
                    cell
    in
    List.map pruneCell cells


removeFixed : List a -> List a -> List a
removeFixed fixedVals possible =
    List.foldl List.Extra.remove possible fixedVals



-- Encode/Decode


encode : Grid -> Encode.Value
encode (Grid grid) =
    Encode.array Cell.encode grid


decoder : Decoder Grid
decoder =
    Decode.map Grid (Decode.array Cell.decoder)



-- Coord Type


type alias Coord =
    { x : Int, y : Int }


coordToIndex : Coord -> Int
coordToIndex coord =
    let
        { x, y } =
            normalizeCoord coord
    in
    (y * 9) + x


normalizeCoord : Coord -> Coord
normalizeCoord coord =
    let
        y =
            if coord.y > 8 then
                8

            else if coord.y < 0 then
                0

            else
                coord.y

        x =
            if coord.x > 8 then
                8

            else if coord.x < 0 then
                0

            else
                coord.x
    in
    { x = x, y = y }


encodeCoord : Coord -> Encode.Value
encodeCoord coord =
    Encode.object
        [ ( "x", Encode.int coord.x )
        , ( "y", Encode.int coord.y )
        ]


coordDecoder : Decoder Coord
coordDecoder =
    Decode.map2 Coord
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)
