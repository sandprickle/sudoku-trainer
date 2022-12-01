port module Sudoku.SolveGrid exposing
    ( Coord
    , SolveGrid
    , coordDecoder
    , decoder
    , empty
    , encode
    , encodeCoord
    , fromJson
    , fromString
    , getByCoord
    , isLegal
    , isSolvable
    , legal
    , load
    , preview
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
import Sudoku.Number as Number exposing (NumSet)


type SolveGrid
    = SolveGrid (Array Cell)



-- New SolveGrid from String


fromString : String -> SolveGrid
fromString str =
    String.trim str
        |> String.left 81
        |> String.padRight 81 '.'
        |> String.toList
        |> List.map Cell.initFromChar
        |> Array.fromList
        |> SolveGrid



-- Validity Checks


solvable : SolveGrid -> Maybe SolveGrid
solvable (SolveGrid grid) =
    let
        givenNumbers =
            Array.filter Cell.isGiven grid
    in
    if Array.length givenNumbers >= 17 then
        Just (SolveGrid grid)

    else
        Nothing


isSolvable : SolveGrid -> Bool
isSolvable grid =
    solvable grid /= Nothing


legal : SolveGrid -> Maybe SolveGrid
legal grid =
    let
        checkGroup : List Cell -> Bool
        checkGroup group =
            group
                |> List.filterMap Cell.getNumber
                |> List.Extra.allDifferent

        rowsOk =
            grid
                |> toRows
                |> List.map checkGroup
                |> List.member False
                |> not

        colsOk =
            grid
                |> toCols
                |> List.map checkGroup
                |> List.member False
                |> not

        boxesOk =
            grid
                |> toBoxes
                |> List.map checkGroup
                |> List.member False
                |> not
    in
    if rowsOk && colsOk && boxesOk then
        Just grid

    else
        Nothing


isLegal : SolveGrid -> Bool
isLegal grid =
    legal grid /= Nothing



-- Get/set by Coord


getByCoord : Coord -> SolveGrid -> Cell
getByCoord coord (SolveGrid grid) =
    Array.get (coordToIndex coord) grid |> Maybe.withDefault Cell.default


setByCoord : Coord -> SolveGrid -> Cell -> SolveGrid
setByCoord coord (SolveGrid grid) newCell =
    Array.set (coordToIndex coord) newCell grid |> SolveGrid



-- Convert to List of Rows, Cols, or Boxes


toRows : SolveGrid -> List (List Cell)
toRows grid =
    List.map (\n -> getRow n grid) (List.range 0 8)


toCols : SolveGrid -> List (List Cell)
toCols grid =
    List.map (\n -> getCol n grid) (List.range 0 8)


toBoxes : SolveGrid -> List (List Cell)
toBoxes grid =
    List.map (\n -> getBox n grid) (List.range 0 8)



-- Get a Specific row, col, or box as a List


getRow : Int -> SolveGrid -> List Cell
getRow rowNum grid =
    List.map (\coord -> getByCoord coord grid) (rowCoords rowNum)


getCol : Int -> SolveGrid -> List Cell
getCol colNum grid =
    List.map (\coord -> getByCoord coord grid) (colCoords colNum)


getBox : Int -> SolveGrid -> List Cell
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
    Array.fromList (List.repeat 81 Cell.default) |> SolveGrid



-- Puzzle Logic


resetPossible : SolveGrid -> SolveGrid
resetPossible (SolveGrid arr) =
    arr
        |> Array.map
            (\cell ->
                case cell of
                    Fixed _ _ ->
                        cell

                    Given _ ->
                        cell

                    Possible _ notes ->
                        Possible Number.setAll notes
            )
        |> SolveGrid



-- Encode/Decode


encode : SolveGrid -> Encode.Value
encode (SolveGrid grid) =
    Encode.array Cell.encode grid


decoder : Decoder SolveGrid
decoder =
    Decode.map SolveGrid (Decode.array Cell.decoder)


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



-- View Helpers


preview : SolveGrid -> Html msg
preview grid =
    let
        rows =
            toRows grid

        viewCell cell =
            Html.td []
                [ Html.div [] [ Html.text (Cell.numberToString cell) ] ]

        viewRow row =
            Html.tr [] (List.map viewCell row)
    in
    Html.table
        [ Html.Attributes.class "puzzle-preview border-2 border-gray-400" ]
        (List.map viewRow rows)