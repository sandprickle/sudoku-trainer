port module Sudoku.Puzzle exposing
    ( Coord
    , Error
    , Pruned
    , Puzzle
    , Unknown
    , Valid
    , check
    , clearNumber
    , decoder
    , empty
    , encode
    , fromString
    , insertNumber
    , load
    , prune
    , receiver
    , rows
    , save
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Sudoku.Cell as Cell exposing (Cell)
import Sudoku.Grid as Grid exposing (Grid)
import Sudoku.Number as Number exposing (Number)


type Puzzle a
    = Puzzle a Grid


type alias Coord =
    Grid.Coord


type Pruned
    = Pruned


type Valid
    = Valid


type Error
    = Error Problem


type Unknown
    = Unknown


type Problem
    = Illegal
    | Unsolvable


rows : Puzzle a -> List (List Cell)
rows (Puzzle _ grid) =
    Grid.toRows grid


prune : Puzzle Valid -> Puzzle Pruned
prune (Puzzle _ grid) =
    Grid.pruneAll grid |> Puzzle Pruned


check : Puzzle a -> Result (Puzzle Error) (Puzzle Valid)
check (Puzzle _ grid) =
    if not (Grid.isSolvable grid) then
        Err (Puzzle (Error Unsolvable) grid)

    else if not (Grid.isLegal grid) then
        Err (Puzzle (Error Illegal) grid)

    else
        Ok (Puzzle Valid grid)


resetPossible : Puzzle Valid -> Puzzle Valid
resetPossible (Puzzle _ grid) =
    Grid.resetPossible grid |> Puzzle Valid


clearNumber : Puzzle a -> Coord -> Puzzle a
clearNumber (Puzzle _ grid) coord =
    Debug.todo "clearNumber"


insertNumber :
    Number
    -> Coord
    -> Puzzle Valid
    -> Result (Puzzle Error) (Puzzle Valid)
insertNumber num coord (Puzzle _ grid) =
    Debug.todo "insertNumber"


fromString : String -> Puzzle Unknown
fromString str =
    Puzzle Unknown (Grid.fromString str)



-- HINTS


type Hint
    = Hint Pattern Int


type Pattern
    = NakedSingle
    | HiddenSingle
    | XWing
    | NakedPair


requestHint : Puzzle Pruned -> Pattern -> Maybe Hint
requestHint (Puzzle _ grid) pattern =
    Debug.todo "requestHint"


empty : Puzzle Error
empty =
    Puzzle (Error Unsolvable) Grid.empty



-- Encode/Decode


encode : Puzzle a -> Encode.Value
encode (Puzzle _ grid) =
    Encode.object
        [ ( "grid", Grid.encode grid ) ]


decoder : Decode.Decoder (Puzzle Unknown)
decoder =
    Decode.field "grid" Grid.decoder
        |> Decode.map (Puzzle Unknown)



-- Ports


port savePuzzle : Decode.Value -> Cmd msg


port loadPuzzle : () -> Cmd msg


port puzzleReceiver : (Decode.Value -> msg) -> Sub msg


save : Puzzle a -> Cmd msg
save puzzle =
    savePuzzle (encode puzzle)


load : Cmd msg
load =
    loadPuzzle ()


receiver : (Maybe (Puzzle Unknown) -> msg) -> Sub msg
receiver fromPuzzle =
    let
        fromJson json =
            case Decode.decodeValue decoder json of
                Ok puzzle ->
                    Just puzzle

                Err _ ->
                    Nothing
    in
    puzzleReceiver (\json -> fromJson json |> fromPuzzle)
