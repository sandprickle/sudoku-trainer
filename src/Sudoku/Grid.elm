module Sudoku.Grid exposing
    ( Coord
    , Grid
    , boxCoords
    , colCoords
    , compareCoord
    , coordDecoder
    , coordMap
    , coordToIndex
    , decoder
    , encode
    , encodeCoord
    , getBox
    , getByCoord
    , getByIndex
    , getCol
    , getRow
    , indexToCoord
    , init
    , isLegal
    , isSolvable
    , map
    , rowCoords
    , setByCoord
    , setByIndex
    , toArray
    , toBoxes
    , toCols
    , toList
    , toRows
    )

{-| A generic 9 x 9 grid.
Provides helpers for accessing individual cells or entire rows, columns, and boxes.
This is used by the `Sudoku.Grid` and `Sudoku.NewGrid` modules,
which use different types for the grid cells.
-}

import Array exposing (Array)
import Array.Extra
import Json.Decode as Json exposing (Decoder)
import Json.Encode as Encode
import List.Extra
import Sudoku.Number exposing (Number)


{-| A generic 9x9 grid
-}
type Grid a
    = Grid
        { default : a
        , array : Array a
        }


{-| Initialize a `Grid` with a default element.
All cells in the grid will contain the default element.
-}
init : a -> Grid a
init default =
    Grid
        { default = default
        , array = Array.repeat 81 default
        }


map : (a -> b) -> Grid a -> Grid b
map fn (Grid { default, array }) =
    Grid
        { default = fn default
        , array = Array.map fn array
        }


coordMap : (Coord -> a -> b) -> Grid a -> Grid b
coordMap fn (Grid { default, array }) =
    Grid
        { default = fn { x = 0, y = 0 } default
        , array =
            Array.indexedMap
                (\index elem ->
                    fn (indexToCoord index) elem
                )
                array
        }


isLegal : (a -> Maybe Number) -> Grid a -> Bool
isLegal getNumber grid =
    let
        checkGroup : List a -> Bool
        checkGroup group =
            group
                |> List.filterMap getNumber
                |> List.Extra.allDifferent

        ok : (Grid a -> List (List a)) -> Bool
        ok toGroup =
            grid
                |> toGroup
                |> List.map checkGroup
                |> List.member False
                |> not
    in
    ok toRows && ok toCols && ok toBoxes


isSolvable : (a -> Maybe Number) -> Grid a -> Bool
isSolvable getNumber (Grid { array }) =
    let
        givenNumbers =
            Array.Extra.filterMap getNumber array
    in
    Array.length givenNumbers >= 17



-- Internal Helpers


setArray : Grid a -> Array a -> Grid a
setArray (Grid { default }) newArray =
    Grid
        { array = newArray
        , default = default
        }



-- Get/Set by Coord


getByCoord : Coord -> Grid a -> a
getByCoord coord (Grid { array, default }) =
    Array.get (coordToIndex coord) array
        |> Maybe.withDefault default


setByCoord : Coord -> Grid a -> a -> Grid a
setByCoord coord grid newCell =
    let
        (Grid { array }) =
            grid
    in
    Array.set (coordToIndex coord) newCell array
        |> setArray grid



-- Get/Set by index


getByIndex : Int -> Grid a -> a
getByIndex index (Grid { array, default }) =
    Array.get index array
        |> Maybe.withDefault default


setByIndex : Int -> Grid a -> a -> Grid a
setByIndex index grid newCell =
    let
        (Grid { array }) =
            grid
    in
    Array.set index newCell array
        |> setArray grid



-- Convert to List of Rows, Cols, or Boxes


toRows : Grid a -> List (List a)
toRows grid =
    List.map (\n -> getRow n grid) (List.range 0 8)


toCols : Grid a -> List (List a)
toCols grid =
    List.map (\n -> getCol n grid) (List.range 0 8)


toBoxes : Grid a -> List (List a)
toBoxes grid =
    List.map (\n -> getBox n grid) (List.range 0 8)



-- Get a Specific row, col, or box as a List


getRow : Int -> Grid a -> List a
getRow rowNum grid =
    List.map (\coord -> getByCoord coord grid) (rowCoords rowNum)


getCol : Int -> Grid a -> List a
getCol colNum grid =
    List.map (\coord -> getByCoord coord grid) (colCoords colNum)


getBox : Int -> Grid a -> List a
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



-- Convert to list of cells


toArray : Grid a -> Array a
toArray (Grid { array }) =
    array


toList : Grid a -> List a
toList grid =
    toArray grid |> Array.toList



-- Encode/Decode


{-| Convert a `Grid` to a JSON `Value`.
Requires an encoder for the cell type.
-}
encode : (a -> Json.Value) -> Grid a -> Json.Value
encode encodeCell (Grid { array, default }) =
    Encode.object
        [ ( "default", encodeCell default )
        , ( "array", Encode.array encodeCell array )
        ]


{-| A decoder for a `Grid`.
Must provide a decoder for the cell type
-}
decoder : Decoder a -> Decoder (Grid a)
decoder cellDecoder =
    Json.map2
        (\default array ->
            Grid { default = default, array = array }
        )
        (Json.field "default" cellDecoder)
        (Json.field "array" (Json.array cellDecoder))



-- Coord Type & Helpers


{-| An x,y coordinate in the grid.
x and y values are zero-indexed.
-}
type alias Coord =
    { x : Int
    , y : Int
    }


coordToIndex : Coord -> Int
coordToIndex coord =
    let
        normalize index =
            if index > 8 then
                8

            else if index < 0 then
                0

            else
                index

        x =
            normalize coord.x

        y =
            normalize coord.y
    in
    (y * 9) + x


indexToCoord : Int -> Coord
indexToCoord index =
    let
        normalizedIndex =
            if index < 0 then
                0

            else if index > 80 then
                80

            else
                index
    in
    { x = modBy 9 normalizedIndex
    , y = normalizedIndex // 9
    }


compareCoord : Coord -> Coord -> Order
compareCoord coord1 coord2 =
    case compare coord1.x coord2.x of
        LT ->
            LT

        GT ->
            GT

        EQ ->
            case compare coord1.y coord2.y of
                LT ->
                    LT

                GT ->
                    GT

                EQ ->
                    EQ


encodeCoord : Coord -> Json.Value
encodeCoord coord =
    Encode.object
        [ ( "x", Encode.int coord.x )
        , ( "y", Encode.int coord.y )
        ]


coordDecoder : Decoder Coord
coordDecoder =
    Json.map2 Coord
        (Json.field "x" Json.int)
        (Json.field "y" Json.int)
