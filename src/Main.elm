module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, div, h1, p, span, table, td, text, tr)
import Html.Attributes exposing (class, href)
import Json.Decode as Json
import Sudoku.Cell as Cell exposing (Cell)
import Sudoku.Grid as Grid exposing (Grid)
import UI


type alias Flags =
    Json.Value


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { puzzle : Maybe Grid
    , view : View
    }


type Msg
    = NoOp


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { puzzle = Grid.fromJson flags
      , view = Start
      }
    , Cmd.none
    )


type View
    = Start


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Document Msg
view model =
    { title = "Sudoku Trainer"
    , body = UI.layout []
    }


viewPuzzlePreview : Grid -> Html Msg
viewPuzzlePreview grid =
    let
        rows =
            Grid.toRows grid

        viewCell index cell =
            td [ class "h-6 w-6  text-sm" ]
                [ div
                    [ class "flex justify-center, items-center" ]
                    [ text (Cell.numberToString cell) ]
                ]

        viewRow index row =
            tr [] (List.indexedMap viewCell row)
    in
    table [ class "puzzle-preview border-2 border-gray-400" ] (List.indexedMap viewRow rows)
