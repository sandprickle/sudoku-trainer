module Main exposing (main)

import Browser exposing (Document)
import Html
    exposing
        ( Html
        , a
        , div
        , h1
        , header
        , main_
        , p
        , span
        , table
        , td
        , text
        , tr
        )
import Html.Attributes
    exposing
        ( class
        , href
        , target
        )
import Json.Decode as Json
import Sudoku.Cell as Cell exposing (Cell)
import Sudoku.Grid as Grid exposing (Grid)
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr


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


type View
    = Start


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    let
        ( title, body ) =
            case model.view of
                Start ->
                    ( "Sudoku Trainer"
                    , [ viewStart model.puzzle ]
                    )
    in
    { title = title
    , body = layout body
    }


viewStart : Maybe Grid -> Html Msg
viewStart currentPuzzle =
    div [] [ text "start" ]


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


layout : List (Html msg) -> List (Html msg)
layout children =
    [ header [ class "flex items-center justify-between px-8 py-4" ]
        [ h1
            [ class "text-3xl text-primary font-bold" ]
            [ text "Sudoku Trainer" ]
        , a
            [ href "https://github.com/sandprickle/sudoku-trainer"
            , target "_blank"
            , class "text-primary"
            ]
            [ githubIcon ]
        ]
    , main_ [] children
    ]


githubIcon : Html msg
githubIcon =
    svg
        [ SvgAttr.width "24"
        , SvgAttr.height "24"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        , SvgAttr.class "feather feather-github"
        ]
        [ path
            [ SvgAttr.d
                "M9 19c-5 1.5-5-2.5-7-3m14 6v-3.87a3.37 3.37 0 0 0-.94-2.61c3.14-.35 6.44-1.54 6.44-7A5.44 5.44 0 0 0 20 4.77 5.07 5.07 0 0 0 19.91 1S18.73.65 16 2.48a13.38 13.38 0 0 0-7 0C6.27.65 5.09 1 5.09 1A5.07 5.07 0 0 0 5 4.77a5.44 5.44 0 0 0-1.5 3.78c0 5.42 3.3 6.61 6.44 7A3.37 3.37 0 0 0 9 18.13V22"
            ]
            []
        ]
