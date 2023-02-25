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
import Sudoku.Solve.Puzzle as Puzzle exposing (Puzzle)


type alias Flags =
    Json.Value


type alias Model =
    { currentPuzzle : Maybe Puzzle }


type Msg
    = UpdatedPuzzle (Maybe Puzzle)


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    ( { currentPuzzle = Puzzle.fromJson flags }, Cmd.none )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        UpdatedPuzzle puzzle ->
            ( { model | currentPuzzle = puzzle }, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Puzzle.receiver UpdatedPuzzle
