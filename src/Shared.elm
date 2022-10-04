module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    )

import Json.Decode as Json
import Request exposing (Request)
import Sudoku.Puzzle as Puzzle exposing (Puzzle)


type alias Flags =
    Json.Value


type alias Model a =
    { currentPuzzle : Maybe (Puzzle a) }


type Msg a
    = UpdatedPuzzle (Maybe (Puzzle a))


init : Request -> Flags -> ( Model Puzzle.Unknown, Cmd (Msg a) )
init _ flags =
    let
        currentPuzzle =
            case Json.decodeValue Puzzle.decoder flags of
                Ok puzzle ->
                    Just puzzle

                Err _ ->
                    Nothing
    in
    ( { currentPuzzle = currentPuzzle }, Cmd.none )


update : Request -> Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update _ msg model =
    case msg of
        UpdatedPuzzle puzzle ->
            ( { model | currentPuzzle = puzzle }, Cmd.none )


subscriptions : Request -> Model a -> Sub (Msg Puzzle.Unknown)
subscriptions _ _ =
    Puzzle.receiver UpdatedPuzzle
