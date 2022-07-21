module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    )

import Json.Decode as Json
import Request exposing (Request)
import Sudoku.Grid as Grid exposing (Grid)


type alias Flags =
    Json.Value


type alias Model =
    { puzzle : Grid }


type Msg
    = NoOp


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    ( { puzzle = Grid.fromJson flags }, Cmd.none )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
